# Commands

*Commands* are issued by you, the user of Komposition, to navigate and
modify, the timeline and other views of the application. The following table
lists some of the central commands you can issue, and their key bindings.
Most commands can also be issued by using the top-level menu bar.

| Command   | Mode                      | Key Binding                           | Description                                                                  |
|-----------|---------------------------|---------------------------------------|------------------------------------------------------------------------------|
| Help      | <em>All</em>              | <kbd>?</kbd>                          | Display key bindings help in the current mode                                |
| Split     | Timeline                  | <kbd>s</kbd>                          | Split the currently focused composition, if possible                         |
| Join      | Timeline                  | <kbd>m</kbd>                          | Merge the currently focused and the right-adjacent composition, if possible  |
| Delete    | Timeline                  | <kbd>d</kbd>                          | Delete the currently focused composition                                     |
| Import    | Timeline                  | <kbd>i</kbd>                          | Start a new asset import                                                     |
| Preview   | Timeline                  | <kbd>Space</kbd>                      | Preview the currently focused composition                                    |
| Undo      | Timeline                  | <kbd>u</kbd>                          | Undo the last command                                                        |
| Redo      | Timeline                  | <kbd>Ctrl</kbd> + <kbd>r</kbd>        | Redo the last undone command                                                 |
| Exit      | Timeline                  | <kbd>q</kbd>                          | Exit the Komposition application                                             |
| Cancel    | Library, Import           | <kbd>q</kbd>                          | Cancel/exit the current mode                                                 |

There are more commands not listed in this table. Press <kbd>?</kbd> in
Komposition to see the full listing.

## Key Sequences

To *add* clips and gaps, there are key sequence bindings available. To
add after, you generally start by pressing <kbd>A</kbd> and then a key
for the type of clip or gap you want to add. To add before, start with
the <kbd>a</kbd> key. Clips are specified with the <kbd>c</kbd> key,
and gaps with the <kbd>g</kbd> key.

When a video or audio track, or anything within a track is focused,
you press two keys:

1. insert after (<kbd>a</kbd>) or insert before (<kbd>A</kbd>)
2. clip (<kbd>c</kbd>) or gap (<kbd>g</kbd>)

When a parallel is focused, you need to also specify if it's the video track
or audio track you want to add before or append to. The full key sequence then
consists of three keys:

1. insert before (<kbd>A</kbd>) or insert after (<kbd>a</kbd>)
2. video (<kbd>v</kbd>) or audio track (<kbd>a</kbd>)
3. clip (<kbd>c</kbd>) or gap (<kbd>g</kbd>)

There are variations for adding at the leftmost position, and at the
rightmost position, but those are currently only available in the menu
bar. And again, all key sequences are not listed in this
document. Press <kbd>?</kbd> in Komposition to see the full listing.
