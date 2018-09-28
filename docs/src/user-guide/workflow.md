# Workflow

Komposition is a highly opinionated piece of software, specialized at editing
screencasts. As such, it prescribes a specific recording and editing workflow.
To reach optimal results and have a nice experience doing so, please follow
the guidelines outlined below.

## Recording

It all begins with the recording. For Komposition to be able to automatically
classify the parts of your screencast, and for you to be able to compose
those parts in a fine-grained and effortless manner, your recording needs to
be done in a certain way.

**Write a detailed script before recording anything.**
:   This is paramount for the Komposition workflow. Also, it's generally good
    advice that will save you loads of time, even if you're not usingKomposition.
    Your script should be detailed enough for you to record video and audio
    separately based off it.

**Record video and audio separately.**
:   Video and audio recorded simultaneously is hard to separate and work with
    independently. The microphone easily picks up the sound of you typing or
    clicking the mouse, making the audio track inherently tied to the video track.
    If you're not a native English speaker, recording
    Finally, it's easier to get a clean audio track without computer fans humming
    if you record it separately.

**Take long breaks, two seconds or more, between the parts in your screencast.**
:   For the scene and sentence classifiers to pick up the different segments,
    you need to take long explicit breaks. A "part" of a screencast should be a short
    sequence of actions, and the corresponding sentence, or couple of sentences, describing
    that action. The more fine-grained you make your parts, the more control and comfort
    you'll have when editing the screencast in Komposition.

## Composing Parts

Now that you have recored your video and audio, you can
[import](concepts/importing.md) using automatic classification. This will
give you a [library](concepts/library.md) loaded with fine-grained parts of
video and audio. Using the [commands](concepts/commands.md) you compose your
parts in [sequences and parallels](concepts/timeline.md) into a complete
screencast.