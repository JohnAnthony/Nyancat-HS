# nyancat

An obstensively charming but later very annoying screensaver based on
the Pop Tart / nyan cat fad of late 2010/2011.

## Usage


```
Flags:
    -h --help
           Help information. Points here or to the manpage

    -d --data-set <str>
           Choose your dataset. Included are "default" and "freedom"

    -f --fullscreen 
    -F --windowed --no-fullscreen
           Switch between fullscreen (default) and windowed modes

	-m --music
    -M --no-music --mute
           Music on (default) or off

    -s --hw-surface
    -S --sw-surface --no-hw-surface
           Choose between a hardware surface (default) and software
           surface

    -x --width <#>
    -y --height <#>
           Set the width and height of the drawing surface. These
           default to 800x600 in windowed mode or your natice
           resolution in fullscreen mode
```

## Known Bugs

There appears to be some form of nasty bug in the Haskell SDL.Mixer
bindings that cause music to halt if SDL.Mixer.freeMusic isn't later
going to be called and a segfault or linked list corruption error if
it IS and you exit the program early. The program has taken up the
second option as it is the only way to remain usable and only has an
effect in some cases.

If you have any fix for this, please do contact me.

## License
(Copied from LICENSE file)

Copyright (C) 2011, 2012 John Anthony

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
