# Instastream
Erlang n2o application that streams Instagrams photos to your browser
##Setup
You need Erlang R17
###Get code and dependencies

```
git clone https://github.com/etataurov/instastream
cd instastream
./rebar get-deps
```
###Configure
You need to put yours Instagram API CLIENT_ID and CLIENT_SECRET into
`apps/instagram/src/instagram.app.src`

Also you should specify YOUR-HOST in callback_url. It should be an accessible IP address or hostname of host where you start the app.

##Start
Start the application

```
./rebar co && erl -name "web@$(hostname)" -pa deps/*/ebin -pa apps/*/ebin -boot start_sasl -s web_app start
```
Visit [http://127.0.0.1:8080](http://127.0.0.1:8080)