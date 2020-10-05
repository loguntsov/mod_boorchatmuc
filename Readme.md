# Customised MUC IQ's for Boorchat

## Description

This module replaces response for standard request of room discovery IQ ( https://xmpp.org/extensions/xep-0045.html#disco-rooms ):
```
<iq from='hag66@shakespeare.lit/pda'
    id='zb8q41f4'
    to='chat.shakespeare.lit'
    type='get'>
  <query xmlns='http://jabber.org/protocol/disco#items'/>
</iq>
```

Response includes counter of members and online users. Also it includes affiliation status and role of current user for this room.

Example of response:

```
<iq from='chat.shakespeare.lit'
    id='zb8q41f4'
    to='hag66@shakespeare.lit/pda'
    type='result'>
  <query xmlns='http://jabber.org/protocol/disco#items'>
    <item jid="dcc566e7-5d8b-4a71-a70b-cc828e10ab34@conference.boorchat.ru" name="укенг (0)">
        <members total="0" online="0"/>
        <role value="none"/>
        <affiliation value="none"/>
    </item>
    <item jid="test@conference.boorchat.ru" name="test (1)">
        <members total="1" online="1"/>
        <role value="moderator"/>
        <affiliation value="owner"/>
    </item>
    ....
    <item jid='coven@chat.shakespeare.lit'
          name='A Dark Cave'>
        ....
    </item>
  </query>
</iq>
```

### Ejabberd 20

# Installation Ejabberd from sources

## Installation dependencies

```
apt install gcc libssl-dev libexpat1-dev libyaml-dev g++ zlib1g-dev
```

# Author

Sergey Loguntsov mailto: [loguntsov][gmail.com]

# License

MIT

