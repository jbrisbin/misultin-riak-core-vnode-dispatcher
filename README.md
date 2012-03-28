# Misultin Riak Core VNode Dispatcher

This isn't so much a reusable application as an example of how to dispatch a web request 
into a riak_core vnode.

There is a vnode called `hello_world_vnode` that responds with a misultin response. Right 
now it's:

    Req:ok([{"Content-Type", "text/plain"}], <<"Hello World!">>)

It dispatches into your vnode based on a tuple of `{Method, Path, Req}`. To respond to misultin 
requests in your vnode, you'll want to implement `handle_command` functions:

    %% To handle root path: '/'
    handle_command({'GET', [], Req}, _Sender, State) ->
      Response = Req:ok([{"Content-Type", "text/plain"}], <<"Hello World!">>),
      {reply, {ok, Response}, State};
    
    %% To handle '/services/myservice' using path matching
    handle_command({'GET', ["services", Svc], Req}, _Sender, State) ->
      ...

If you return a `{reply, {ok, Response}, State}`, the dispatcher will send that back on to the client. 
Although a `{noreply, State}` response is valid as far as riak_core is concerned, not sending a 
response back to the client at all will result in an eventual timeout. That's probably a Bad Thing 
in general. There is an HTTP status code for doing long-running, delayed processing if you need that 
sort of functionality. It's code 202 - Accepted.

### How do I use it?

To test out this code, build it by typing `make` then issue the command `./node1.sh`. Send a web request 
using curl or somesuch:

    curl -v http://localhost:3000/

You should get "Hello World!" back.

To test the clustering capabilities, open a second console window and start another node using this `./node2.sh` 
script. Once started, the two nodes should be clustered together and in a riak_core ring together. Send 
another request like above to make sure both instances can respond.

You know have a clustered web application! If you kill one of the processes, your application will continue 
to function (provided you send HTTP traffic to the other misultin server, of course...this is where an 
haproxy or other load-balancer would come in).
