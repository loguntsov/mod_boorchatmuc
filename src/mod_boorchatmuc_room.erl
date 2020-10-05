-module(mod_boorchatmuc_room).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

%% Modified logic (and not exported) from mod_muc_room.erl

%% API
-export([
  get_disco_item/4,

  get_total_members/1,
  get_online_members/1
]).

-include("translate.hrl").

-include("xmpp.hrl").
-include("mod_muc_room.hrl").

-type state() :: #state{}.



%% Replacement handle_sync_event({get_disco_item, Filter, JID, Lang, Time}, _From, StateName, StateData) ->
get_disco_item(StateData, Filter, JID, Lang) ->
  Len = maps:size(StateData#state.nicks),
  Reply = case (Filter == all) or (Filter == Len) or ((Filter /= 0) and (Len /= 0)) of
    true ->
      get_roomdesc_reply(JID, StateData,
        get_roomdesc_tail(StateData, Lang));
    false ->
      false
  end,
  Reply.

-spec get_roomdesc_reply(jid(), state(), binary()) -> {item, binary()} | false.
get_roomdesc_reply(JID, StateData, Tail) ->
  IsOccupantOrAdmin = mod_muc_room:is_occupant_or_admin(JID, StateData),
  if (StateData#state.config)#config.public or
    IsOccupantOrAdmin ->
    if (StateData#state.config)#config.public_list or
      IsOccupantOrAdmin ->
      {item, <<(get_title(StateData))/binary,Tail/binary>>};
      true -> {item, get_title(StateData)}
    end;
    true -> false
  end.

-spec get_title(state()) -> binary().
get_title(StateData) ->
  case (StateData#state.config)#config.title of
    <<"">> -> StateData#state.room;
    Name -> Name
  end.


get_total_members(StateData) ->
  maps:size(StateData#state.users).

get_online_members(StateData) ->
  maps:fold(fun(_JID, UserInfo, Acc) ->
    case UserInfo#user.role of
      none -> Acc;
      _ ->
        case UserInfo#user.last_presence of
          #presence{ type = available } -> [ UserInfo#user.nick | Acc ];
          _ -> Acc
        end
    end
  end, [], StateData#state.users).

-spec get_roomdesc_tail(state(), binary()) -> binary().
get_roomdesc_tail(StateData, Lang) ->
  Desc = case (StateData#state.config)#config.public of
    true -> <<"">>;
    _ -> translate:translate(Lang, ?T("private, "))
  end,
  Len = maps:size(StateData#state.nicks),
  <<" (", Desc/binary, (integer_to_binary(Len))/binary, ")">>.




