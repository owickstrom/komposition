# Commands

*Commands* are issued by you, the user of Komposition, to navigate and
modify, the timeline and other views of the application. The following table
lists some of the central commands you can issue, and their key bindings.
Most commands can also be issued by using the top-level menu bar.

| Command   | Mode                      | Key Binding                       | Description                                               |
|-----------|---------------------------|-----------------------------------|-----------------------------------------------------------|
| Help      | *                         | <kbd>?</kbd>                      | Display key bindings help in the current mode             |
| Split     | Timeline                  | <kbd>s</kbd>                      | Split the currently focused composition, if possible      |
| Delete    | Timeline                  | <kbd>d</kbd>                      | Delete the currently focused composition                  |
| Import    | Timeline                  | <kbd>i</kbd>                      | Start a new asset import                                  |
| Render    | Timeline                  | <kbd>r</kbd>                      | Render the project to a video file                        |
| Preview   | Timeline                  | <kbd>&lt;space&gt;</kbd>          | Preview the currently focused composition                 |
| Exit      | Timeline                  | <kbd>q</kbd>                      | Exit the Komposition application                          |
| Cancel    | Library, Import           | <kbd>q</kbd>                      | Cancel/exit the current mode                              |

There are more commands not listed in this table. Press <kbd>?</kbd> in
Komposition to see the full listing.

<!--
TODO: describe these somehowe:

  FocusCommand :: FocusCommand -> Command TimelineMode
  JumpFocus :: Focus SequenceFocusType -> Command TimelineMode
  InsertCommand :: InsertType -> InsertPosition -> Command TimelineMode
-->