# Navigation

To navigate the timeline in a Komposition project, you modify the *focus*.
The focus describes the current part of the timeline you're focusing. It is
modified using movement commands, such as the "Focus Left" and "Focus Up"
commands.

Visually, it's represented with a white border around the focused
timeline part. For example:

* Focusing a sequence:

    ![A focused sequence](sequence.png)

* Focusing a parallel:

    ![A focused parallel](parallel.png)

* Focusing a video clip:

    ![A focused video clip](video-clip.png)

## Key Bindings

Navigation is mean to be keyboard-driven, in that you use your arrow keys or
the <kbd>h</kbd><kbd>j</kbd><kbd>k</kbd><kbd>l</kbd> keys to move around,
like in [Vim](https://www.vim.org/). You can also click any clip or gap in a
parallel to move directly to it.

!!! note
    There's currently no way to focus sequences or parallels using the mouse,
    but this should be addressed in the future.