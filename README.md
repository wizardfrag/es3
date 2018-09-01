# es3

Erlang Simple Storage Service

## Build

    $ make compile

## Test
    
    $ make test
    
## Summary

This system uses `mnesia` for metadata storage and distribution. 
It uses standard erlang messaging to pass chunks between nodes.


    
## Troubleshooting

### `make compile` fails.

Try using `rebar3 update` first. Or set the `REBAR` environment variable to the location of your rebar3 command

