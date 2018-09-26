Timeline
========

A FastCut *project* has a *timeline*, where you place your video and
audio parts you want to render to a video file. The timeline is a
tree structure of *compositions*, and FastCut enforces what kinds of
compositions can occur within other compositions:

* A timeline contains one or more child *sequences*.
* A sequence contains one or more child *parallels*, where each child
  is played sequentially in order.
* A parallel contains a video track and an audio track, where the
  video and audio tracks are played in parallel (simultaneously). The
  longest track defines the length of the parallel.

Sequences and parallels are used to structure logical groups of video
and audio parts into cohesive units, and to synchronize the start of
video and audio. For example, if you want video clip *v1* to start at
exactly the same time as audio clip *a1*, put them both in a parallel.
