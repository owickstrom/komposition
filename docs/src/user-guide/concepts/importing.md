# Importing

To import media files, video or audio, into your [library](library.md), press
the <kbd>i</kbd> key or click _Import Assets_ in the _Project_ top menu. You'll
see a modal dialog.

The first control selects a file to import. Second, you'll see a checkbox.
Check this if you want to automatically classify scenes when importing video,
or automatically classify sentences when importing audio. Finally, click
the "Import" button to start the process. It can take some time, so go grab
a cup of coffee or tea.

## Scene and Sentence Classification

Automatically classifying scenes in a video means that Komposition will
analyze the original video file and figure out in which segments there are
movement and action in the video. Segments where nothing is happening are
disregarded. When imported into the library, each segment with movement ends
up as a separate asset representing that _scene_.

Similarly, audio files can be classified to find spoken sentences.
Komposition will analyze the original audio file and find the segments where
the voiceover audio is silent, and disregard those segments. When imported
into the library, each segment with non-silence ends up as a separate asset
representing that _sentence_, or passage.

## Recording and Workflow

Scene and sentence classification is closely tied to the workflow and
recording techniques prescribed by Komposition. To enjoy good results using
Komposition, make sure to [follow the guidelines](../workflow.md).