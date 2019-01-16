Timeline
========

A project in Komposition has a *timeline*, where you place your video and
audio parts you want to render to a video file. The timeline is a tree
structure, with a fixed depth, that can be described as follows:

* A timeline contains one or more child *sequences*.
* A sequence contains one or more child *parallels*, where each child
  is played sequentially in order.
* A parallel contains a video track and an audio _track_, where the
  video and audio tracks are played in parallel (simultaneously). The
  longest track defines the length of the parallel.

## Sequences

Sequences are used to compose parallels that belong together, forming a
cohesive part, or chapter, of your screencast. Parallels inside a sequence
are played in sequence. By placing video and audio parts in separate
parallels inside a sequence, you can synchronize the start of video and
audio.

## Parallels

Parallels are used to play video and audio tracks in parallel. The parts
within a track play in sequence, until the end of the longest track. This
means that if the audio parts form a longer track than the video parts, the
video will be extended with still frames. Correspondingly, if the video track
is longer, the audio track will be silent in the end.

## Video & Audio Parts

Video and audio parts added to a track are either clips from the
[library](library.md), or *gaps*. A clip is a slice of some original media
file. A video gap is a still frame segment, and an audio gap is simply
silence.

## Examples

* If you want video clip *v1* to start at exactly the same time as
  audio clip *a1*, put them both in the beginning of a parallel.
* If you want video clip *v1* and audio clip *a1* to play simultaneously, and when
  they end play video clip *v2*, put *v1* and *a1* in one parallel followed by
  a parallel containing only *v2*.
* If you want video clip *v1* to play with silence, and then play audio clip
  *a1* together with video clip *v2*, put *v1* in a parallel, and *v2* and *a1*
  in another parallel.
* If you want to play video clip *v1* together with audio clip *a1*, but start
  *a1* after two seconds, put *v1*, a two second audio gap, and *a1* in a
  parallel.

## Adjusting Timeline Parts

The sidebar shows properties of the focused timeline part, and for
things like video and audio clips, it allows you to adjust certain
properties of those timeline parts. You may change the start and end
position within the original media of a video clip, or adjust the
playback speed.
