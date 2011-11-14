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

That depends on your use case. I would start with your own riak_core application that 
already has your own vnodes configured and started in it. You would then copy out the 
`hello_world_vnode_dispatcher.erl` file and put that into your application (it's a gen_server). 
You'll want to edit the the `loop` function to maybe parameterize the vnode you dispatch to. 
Maybe you want to make that based on mapping a query parameter or path segment to a vnode 
name (I wouldn't use direct vnode names as parameters in your web app as that would expose 
important information about the internal structure of your site to a potential attacker).

You'll also likely want to parameterize the port your app starts on. I'm assuming you're a savvy-
enough OTP-nician to figure out how to do that. This isn't a HOWTO so much as example code 
showing you how I did it. Grab what you need, throw away the rest! :)

### Running this code

To test out this code, build it by typing `make` then running the `./console.sh` script, which 
will start the Erlang console with the right parameters. Once started, send a web request.

    curl -v http://localhost:3000/

You should get "Hello World!" back.