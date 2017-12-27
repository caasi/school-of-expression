# school-of-expression

Try to recreate everything from the book ["the Haskell School of Expression"][SOE] in about 14 days.

[SOE]: http://www.cs.yale.edu/homes/hudak/SOE/

## Notes

You may need to install [XQuartz][XQuartz] and set your `$PATH` to build X11. 

```
# Put this in your .bashrc / .bash_profile.
export LIBRARY_PATH="/opt/X11/lib:$LIBRARY_PATH"
```

Check this issue for more information: https://github.com/xmonad/X11/issues/24

[XQuartz]: https://www.xquartz.org/

## ToDo

  * modules from every chapters
    * [x] A Module of Shapes: Part I
    * [x] Simple Graphics
      * [x] `Graphics.SOE` in [HGL][HGL] is still working!
    * [x] Shapes II: Drawing Shapes
    * [x] Polymorphic and Higher-Order Functions
    * [x] Shapes III: Perimeters of Shapes
    * [ ] Trees
    * [x] A Module of Regions
    * [ ] More About High-Order Functions
    * [x] Drawing Regions
    * [ ] Proof by Induction
    * [ ] Qualified Types
    * [x] A Module of Simple Animations
    * [ ] Programming With Streams
    * [ ] A Module of Reactive Animations
    * [ ] Communicating With the Outside World
    * [ ] Rendering Reactive Animations
    * [ ] Higher-Order Types
    * [ ] An Imperative Robot Language
    * [ ] Interpreting Functional Music
    * [ ] From Performance to MIDI
    * [ ] A Tour of the PreludeList Module
    * [ ] A Tour of Haskell's Standard Type Classes

[HGL]: https://hackage.haskell.org/package/HGL
