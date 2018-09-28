# Library

The library contains all your import media assets. Depending on how you
import assets, and if it's video or audio, they are stored differently.

## Video Assets

Original video files are not copied into the project directory when imported
into the library. Instead, the library only stores the file path and metadata
about the original video file.

!!! warning
    As the video asset is referring to an original somewhere on your
    filesystem, if you move or delete the original file it will break
    Komposition. A file path resolution mechanism should be added in a
    future version.

In addition to storing the metadata about the original file, a proxy media
file is created when importing. This is lower-resolution video file that is
used in scene classification, and when previewing, to improve performance
and keep memory usage down.

Video assets produced by the scene classifier are stored in a similar way.
They consist of the original file path and the start and end timestamps of
the classified scene.

## Audio Assets

Like video assets, regular audio assets consist of a file path specifying the
original audio file, and metadata. However, if you import with sentence
classification, the sentences are split into new audio files stored in the
project working directory.

!!! note
    Sentence classification producing new audio files in the project working
    directory is a technical consequence of the audio classification
    implementation. In a future version of Komposition it should work just
    as video scene classification, i.e. not producing new audio files, but
    referring to the original with start and end timestamp.