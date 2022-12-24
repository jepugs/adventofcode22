# adventofcode22

Advent of Code 2022, in Clojure. I was using this as an opportunity to learn
Clojure for the first time, so don't expect high quality code.


## Usage

If you wanna run it, I recommend you just run it with leiningen. You can use the
following invocation from the project directory:

    $ lein run <day> <part> <input-file>

where `<day>` is the day number (1-25, not all implemented yet), `<part>` is the
part of the problem (1 or 2) and `<input-file>` is the input. I've included my
input files in the project directory, so you can do for instance

    $ lein run 19 2 input19
    
It should work with anyone's input files, but do let me know if you find a bug
on your input.


## License

Copyright © 2022 Jack Pugmire

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
