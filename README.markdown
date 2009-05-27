# Natter

Natter is an XMPP erlang library. It currently only supports IQ stanzas and has no support for messages, roster, and just enough presence support to be able to connect.

# Building Natter

## If you've built erlang from source, then do the following:

<pre>
autoreconf --install
./configure
make
sudo make install
</pre>

This will install natter at: __/usr/local/lib/erlang/lib/__

## If you are using Ubuntu and installed erlang via apt-get:

<pre>
autoreconf --install
./configure --prefix=/usr/
make
sudo make install
</pre>

This will install natter at: __/usr/lib/erlang/lib/__

# Getting Started

## Connecting

<pre>
Config = [{host, "localhost"}, {user, "foo"}, {password, "bar"}, {resource, "foobar"}].
{ok, Cn} = natter_connection:start_link(Config).
</pre>

## Receiving XMPP Messages

An exchange routes packets to interested processes. A process can tell natter that he is interested in hearing all messages that come in (default_exchange) or that he is interested in hearing messages that go to a particular JID.

### Default Exchange

<pre>
Config = [{host, "localhost"}, {user, "foo"}, {password, "bar"}, {resource, "foobar"}].
{ok, Cn} = natter_connection:start_link(Config).
natter_connection:register_default_exchange(self(), Cn).
</pre>


### Specific Exchange

<pre>
Config = [{host, "localhost"}, {user, "foo"}, {password, "bar"}, {resource, "foobar"}].
{ok, Cn} = natter_connection:start_link(Config).
natter_connection:register_exchange(Cn, "iq", "bar@localhost", self()).
</pre>


# Internals

3 main modules:

* __natter_connection__: public API. Runs as a supervisor.

* __natter_packetizer__: responsible for dealing with all network traffic. Reads in incoming XML and finds out when you have a complete packet. It then sends the packet to the dispatcher.

* __natter_dispatcher__: figures out where packets should go.

XML Parsing is done using an erlang wrapper around libexpat. Inspired by Jabberlang. Faster than xmerl.

XML is parsed into a tuple:
<pre>
{xmlelement, “iq”, Attrs, Subels}
</pre>

# TODO

* Move away from plain-text authentication

* Support for presence

* Support for message stanzas

* Support for rosters


# Resources/Links

[Kevin Smith presents Natter at the Erlang Factory](http://www.erlang-factory.com/conference/SFBayAreaErlangFactory2009/speakers/KevinSmith)
