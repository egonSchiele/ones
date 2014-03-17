Some features that would be nice to have in a higher level game engine:

- animation. Just something like

    on enterFrame $ do
      map ((+5) . x) tiles

  to move all tiles to the right, for example. This is the big one.

- isStillDown. Right now, if you want to make a character go right, you have to keep pressing the right key repeatedly. It would be nice to have an event that detects that a key is still pressed.

- support for different screens. i.e. screen 1 = intro screen, show instructions. Press space to go to the next screen. etc.

- better support for fonts! Right now it's rubbish.

- hittest, and especially mouse hittest would be great

- easy drag handler

- mouseover popups for debugging, so you can easily view all the properties on a tile by mousing over it, for example.
