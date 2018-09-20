{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.Render.FFmpeg.CommandTest where

import           FastCut.Prelude

import           Test.Tasty.Hspec

import           FastCut.Duration
import           FastCut.Render.FFmpeg.Command

spec_printCommandLineArgs = do
  it "prints command-line args for a multi-chain concat command" $
    let videoStream = StreamSelector (StreamName "video") Nothing Nothing
        audioStream = StreamSelector (StreamName "audio") Nothing Nothing
        filter1a =
          RoutedFilter
            [ StreamSelector (StreamIndex 0) (Just Video) (Just 0)
            , StreamSelector (StreamIndex 1) (Just Video) (Just 0)
            ]
            (Concat 2 1 0)
            []
        filter1b = RoutedFilter [] SetPTSStart [videoStream]
        filter2a =
          RoutedFilter
            [StreamSelector (StreamIndex 2) (Just Audio) (Just 0)]
            (Concat 1 0 1)
            []
        filter2b = RoutedFilter [] AudioSetPTSStart [audioStream]
        chain1 = FilterChain (filter1a :| [filter1b])
        chain2 = FilterChain (filter2a :| [filter2b])
        filterGraph = FilterGraph (chain1 :| [chain2])
        inputs =
          (FileSource "foo.mp4" :|
           [ StillFrameSource "bar.png" 25 10
           , AudioNullSource (durationFromSeconds 4.3)
           ])
        mappings = [videoStream, audioStream]
        format = "mp4"
        output = "bar.mp4"
        command = Command {..}
    in printCommandLineArgs command `shouldBe`
       [ "-i"
       , "foo.mp4"
       , "-loop"
       , "1"
       , "-framerate"
       , "25"
       , "-t"
       , "00:00:10.0"
       , "-i"
       , "bar.png"
       , "-f"
       , "lavfi"
       , "-i"
       , "aevalsrc=0:duration=4.3"
       , "-filter_complex"
       , "[0:v:0][1:v:0]concat=n=2:v=1:a=0,setpts=PTS-STARTPTS[video];[2:a:0]concat=n=1:v=0:a=1,asetpts=PTS-STARTPTS[audio]"
       , "-map"
       , "[video]"
       , "-map"
       , "[audio]"
       , "-f"
       , "mp4"
       , "bar.mp4"
       ]
