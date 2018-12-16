{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.FFmpeg.CommandTest where

import           Komposition.Prelude

import           Test.Tasty.Hspec

import           Komposition.FFmpeg.Command

spec_printCommandLineArgs = do
  it "prints command-line args for a multi-chain concat command" $
    let videoStream = StreamSelector (StreamName "video") Nothing Nothing
        audioStream1 = StreamSelector (StreamName "audio1") Nothing Nothing
        audioStream2 = StreamSelector (StreamName "audio2") Nothing Nothing
        filter1a =
          RoutedFilter
            [ StreamSelector (StreamIndex 0) (Just Video) (Just 0)
            , StreamSelector (StreamIndex 1) (Just Video) (Just 0)
            ]
            (Concat 2 1 0)
            []
        filter1b = RoutedFilter [] (SetPTS PTSStart) [videoStream]
        filter2a =
          RoutedFilter
            [StreamSelector (StreamIndex 2) (Just Audio) (Just 0)]
            (Concat 1 0 1)
            []
        filter2b = RoutedFilter [] (AudioSetPTS PTSStart) [audioStream1]
        filter3a = RoutedFilter [] (AudioEvalSource 4) [audioStream2]
        chain1 = FilterChain (filter1a :| [filter1b])
        chain2 = FilterChain (filter2a :| [filter2b])
        chain3 = FilterChain (pure filter3a)
        filterGraph = Just (FilterGraph (chain1 :| [chain2, chain3]))
        inputs =
          (FileSource "foo.mp4" :|
           [ StillFrameSource "bar.png" 25 10
           ])
        mappings = [videoStream, audioStream1, audioStream2]
        format = Just "mp4"
        output = FileOutput "bar.mp4"
        frameRate = Nothing
        vcodec = Nothing
        acodec = Nothing
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
       , "-filter_complex"
       , "[0:v:0][1:v:0]concat=n=2:v=1:a=0,setpts=PTS-STARTPTS[video];[2:a:0]concat=n=1:v=0:a=1,asetpts=PTS-STARTPTS[audio1];aevalsrc=0:duration=4.0[audio2]"
       , "-map"
       , "[video]"
       , "-map"
       , "[audio1]"
       , "-map"
       , "[audio2]"
       , "-f"
       , "mp4"
       , "bar.mp4"
       ]
