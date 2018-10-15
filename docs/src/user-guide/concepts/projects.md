# Projects

In Komposition, the top-level concept is the _project_. The following are all
part of a project:

* the timeline
* the current focus
* the library with its media assets
* video rendering settings
* proxy media settings
* directories to store intermediate files in

## The Welcome Screen

When launching Komposition, you will be presented with the welcome screen.
From there, you can either create a new project, or opening an existing
project.

When creating a new project, you will be prompted for a directory in which
the project files will be saved. Depending on your operating system's file
selector dialog, you might need to create a directory manually and select
that.

To open an existing project, use the file selector to select the same
directory you specified when creating the project.

## Saving Projects

When a project is open, and you are in the timeline mode, you can save the
current project. Click *Save* in the *Project* menu, and it will be
persisted. Note that [undo and redo](undo-redo.md) information is also
available after saving and loading projects.

!!! note
    The project directory format and file selection dialogs might change
    drastically in the early versions of Komposition, as the current state
    is not ideal. However, project conversion utilities should be added to
    make the transitions smoother.