# Codevember 2016

I use the codesimple "elm in docker" toolset.
To build any day's project I use

`elm make src/${day}.elm`

This produces `index.html` which I can then view or upload.

Each day is src/C11XX.elm, which should contain a 'Main' module.

# C1106 Do not look directly into the lights

This is all about the `spread` function. Not happy with the
`Float -> Float -> Float -> List Float` thing, I thought
the third parameter should be `Int`. This is the first sketch to import
C1105. Will be interested to seed where that goes. This also looks forward
to a Cheer Lights sketch.

# C1105 Carpeted Garage

I needed a good function to control a fisheye lens on a scratch program
(aquarium simulation). I worked out a variation of a cubic function
that worked. This sketch is a tribute to that one. Also tired of wrangling
random seeds, so this one is strictly based on the frame counter.

# C1104 Iberian Rose

This is based on a python script I wrote years ago. In building This
one I learned that List.foldl reverses the original list, sometimes
that is OK. I think there is more I could do with random circles.

Started publishing the .html files on [https://rsbohn.github.io/codevember2016].

# C1103 Battletowers

Using random numbers. Start with the same seed, you get the same shapes.
There are a limited number of battletower shapes, you get a different one
when you reload the page.

# C1102 Colorwheel

Just learning to use the Kwarrtz/render graphics library. A simple color wheel.

# Random Notes
[https://github.com/evancz/elm-markdown]
