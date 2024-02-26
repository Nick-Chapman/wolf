# wolf

Wolfenstein style Ray Caster. Using Haskell/SDL.

### Resources
- _3DSage_ [Make Your Own Raycaster Part 1](https://www.youtube.com/watch?v=gYRrGTC7GtA)
- _Matt Godbold_  [Wolfenstein 3D's map renderer](https://www.youtube.com/watch?v=eOCQfxRQ2pY&t=2s)

### Movement Controls
- `A` : Turn left
- `S` : Move backwards
- `D` : Turn right
- `W` : Move forwards
- `Left Arrow` : Strafe Left
- `Right Arrow` : Strafe Right

### Simulator Controls
- `Escape` : Quit
- `Delete` : Pause/Unpause
- `Shift` : Thrust
- `=` : Increase Scale-Factor
- `-` : Decrease Scale-Factor
- `Up Arrow` : Increase Selected Attribute
- `Down Arrow` : Decrease Selected Attribute
- `Return` : Cycle/Select Next Attribute

```
data AdjustableAttribute =
    FPS | HeightScale | ViewAngle | TileSize
```
