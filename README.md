# epic-serv

epic-serv is a service that runs a restful endpoint to provide various
information about the consumption of epic meat bars.

## Building

To build epic-serv you'll
need [to install stack](https://docs.haskellstack.org/en/stable/README/).  Once
stack is installed you can build, run tests, and install the program using
stack:

 - To build the application use: `stack build`
 - To run the unit tests use: `stack test`
 - To run the application without installing it, use: `stack exec epic-serv`
 - To install the application system wide, run: `stack install`

## Running

epic-serv supports several options.  You can run `epic-serv --help` to see a
list of available options and their descriptions.  NB: If you are running the
application through stack, be sure to include an extra set of dashes (`--`) to
separate out the stack options from the program options: (`stack exec epic-serv
-- --help`)
