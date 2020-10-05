-module(mod_boorchatmuc).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% API

-behaviour(gen_mod).
-export([
  start/2, stop/1,
  mod_opt_type/1, depends/2,
  mod_options/1
]).

%% IQ handlers
-export([
  process_disco_items/1
]).

-define(MOD_MUC, mod_muc).

-define(LAGER, true).
-include("logger.hrl").

-include("xmpp.hrl").
-include("translate.hrl").

start(Host, _Opts) ->

  { ok, _ } = application:ensure_all_started(boorchatmuc),

  %% Remove original moc_muc handler
  gen_iq_handler:remove_iq_handler(ejabberd_local, conference_host(Host), ?NS_DISCO_ITEMS),
  gen_iq_handler:add_iq_handler(ejabberd_local, conference_host(Host), ?NS_DISCO_ITEMS, ?MODULE, process_disco_items).

stop(Host) ->
  proc_lib:spawn(fun() ->
    application:stop(boorchatmuc)
  end),
  %% Remove any handler for disco items
  gen_iq_handler:remove_iq_handler(ejabberd_local, conference_host(Host), ?NS_DISCO_ITEMS),
  ok.


depends(_Host, _Opts) ->
  [{mod_adhoc, hard}].

mod_opt_type(_) ->
  [].

mod_options(_Host) -> [
].

%% ---------------------------------------

-spec process_disco_items(iq()) -> iq().
process_disco_items(#iq{type = set, lang = Lang} = IQ) ->
  Txt = ?T("Value 'set' of 'type' attribute is not allowed"),
  xmpp:make_error(IQ, xmpp:err_not_allowed(Txt, Lang));
process_disco_items(#iq{type = get, from = From, to = To, lang = Lang,
  sub_els = [#disco_items{node = Node, rsm = RSM}]} = IQ) ->
  Host = To#jid.lserver,
  ServerHost = ejabberd_router:host_of_route(Host),
  MaxRoomsDiscoItems = mod_muc_opt:max_rooms_discoitems(ServerHost),
  case iq_disco_items(ServerHost, Host, From, Lang, MaxRoomsDiscoItems, Node, RSM) of
    {error, Err} ->
      xmpp:make_error(IQ, Err);
    {result, Result} ->
      xmpp:make_iq_result(IQ, Result)
  end;
process_disco_items(#iq{lang = Lang} = IQ) ->
  Txt = ?T("No module is handling this query"),
  xmpp:make_error(IQ, xmpp:err_service_unavailable(Txt, Lang)).



-spec iq_disco_items(binary(), binary(), jid(), binary(), integer(), binary(),
    rsm_set() | undefined) ->
  {result, disco_items()} | {error, stanza_error()}.
iq_disco_items(ServerHost, Host, From, Lang, MaxRoomsDiscoItems, Node, RSM)
  when Node == <<"">>; Node == <<"nonemptyrooms">>; Node == <<"emptyrooms">> ->
  Count = count_online_rooms(ServerHost, Host),
  Query = if Node == <<"">>, RSM == undefined, Count > MaxRoomsDiscoItems ->
    {only_non_empty, From, Lang};
    Node == <<"nonemptyrooms">> ->
      {only_non_empty, From, Lang};
    Node == <<"emptyrooms">> ->
      {0, From, Lang};
    true ->
      {all, From, Lang}
  end,
  MaxItems = case RSM of
    undefined ->
      MaxRoomsDiscoItems;
    #rsm_set{max = undefined} ->
      MaxRoomsDiscoItems;
    #rsm_set{max = Max} when Max > MaxRoomsDiscoItems ->
      MaxRoomsDiscoItems;
    #rsm_set{max = Max} ->
      Max
  end,
  {Items, HitMax} = lists:foldr(
    fun(_, {Acc, _}) when length(Acc) >= MaxItems ->
      {Acc, true};
      (R, {Acc, _}) ->
        case get_room_disco_item(R, Query) of
          {ok, Item} -> {[Item | Acc], false};
          {error, _} -> {Acc, false}
        end
    end, {[], false}, get_online_rooms(ServerHost, Host, RSM)),
  ResRSM = case Items of
    [_|_] when RSM /= undefined; HitMax ->
      #disco_item{jid = #jid{luser = First}} = hd(Items),
      #disco_item{jid = #jid{luser = Last}} = lists:last(Items),
      #rsm_set{first = #rsm_first{data = First},
        last = Last,
        count = Count};
    [] when RSM /= undefined ->
      #rsm_set{count = Count};
    _ ->
      undefined
  end,
  XmlEl = #xmlel{
    name = <<"query">>,
    attrs = [
      {<<"xmlns">>,<<"http://jabber.org/protocol/disco#items">>}
    ] ++
      case Node of
      undefined -> [];
      <<>> -> [];
      _ -> [{<<"Node">>, Node}]
    end,
    children = Items ++ case ResRSM of
      undefined -> [];
      _ -> xmpp:encode(ResRSM)
    end
  },
  {result, XmlEl};
iq_disco_items(_ServerHost, _Host, _From, Lang, _MaxRoomsDiscoItems, _Node, _RSM) ->
  {error, xmpp:err_item_not_found(?T("Node not found"), Lang)}.


-spec count_online_rooms(binary(), binary()) -> non_neg_integer().
count_online_rooms(ServerHost, Host) ->
  RMod = gen_mod:ram_db_mod(ServerHost, ?MOD_MUC),
  RMod:count_online_rooms(ServerHost, Host).



-spec get_online_rooms(binary(), binary(), undefined | rsm_set()) ->
  [{binary(), binary(), pid()}].
get_online_rooms(ServerHost, Host, RSM) ->
  RMod = gen_mod:ram_db_mod(ServerHost, ?MOD_MUC),
  RMod:get_online_rooms(ServerHost, Host, RSM).


-spec get_room_disco_item({binary(), binary(), pid()},
    {mod_muc_room:disco_item_filter(),
      jid(), binary()}) -> {ok, disco_item()} |
{error, timeout | notfound}.
get_room_disco_item({Name, Host, Pid}, {Filter, JID, Lang}) ->
  %% TODO: Dirty hack to get full state of room
  { ok, StateData } = mod_muc_room:get_state(Pid),
  case mod_boorchatmuc_room:get_disco_item(StateData, Filter, JID, Lang) of
    {item, Desc} ->
      RoomJID = jid:make(Name, Host),
      Affiliation = atom_to_binary(mod_muc_room:get_affiliation(JID, StateData), utf8),
      Role = atom_to_binary(mod_muc_room:get_role(JID, StateData), utf8),
      {ok, #xmlel{
        name = <<"item">>,
        attrs = [
          {<<"jid">>, jid:to_string(RoomJID)},
          {<<"name">>, Desc}
        ],
        children = [
          #xmlel{
            name = <<"members">>,
            attrs = [
              {<<"total">>, integer_to_binary(mod_boorchatmuc_room:get_total_members(StateData))},
              {<<"online">>, integer_to_binary(length(mod_boorchatmuc_room:get_online_members(StateData)))}
            ]
          },
          #xmlel{
            name = <<"role">>,
            attrs = [{ <<"value">>, Role}]
          },
          #xmlel{
            name = <<"affiliation">>,
            attrs = [{ <<"value">>, Affiliation }]
          }
        ]
      }};
    {error, _} = Err ->
      Err
  end.

%% INTERNAL

conference_host(Host) ->
  <<"conference.", Host/binary>>.
