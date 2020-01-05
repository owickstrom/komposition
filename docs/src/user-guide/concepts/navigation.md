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

* Focusing a video track:

    ![A focused video track](video-track.png)

* Focusing an audio track:

    ![A focused audio track](audio-track.png)

* Focusing a video part:

    ![A focused video part](video-part.png)

* Focusing an audio part:

    ![A focused audio part](audio-part.png)

## Key Bindings

Navigation is meant to be keyboard-driven, in that you use your arrow keys or
the <kbd>h</kbd><kbd>j</kbd><kbd>k</kbd><kbd>l</kbd> keys to move around,
like in [Vim](https://www.vim.org/). Press <kbd>?</kbd> to display the help
and its list of key bindings to learn more.

You can also click any clip or gap in a track to move directly to it.

!!! note
    There's currently no way to focus sequences or parallels using the mouse,
    but this should be addressed in the future.
