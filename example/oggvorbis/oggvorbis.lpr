{
   OggVorbis example - part of libVorbis_dyn

   Copyright 2023 Ilya Medvedkov

   In this example, pcm audio data is recorded by OpenAL, encoded and saved
   to a file in vorbis-ogg format in streaming mode.
   Then the saved file is opened, audio data is read and decoded, then played
   by OpenAL with buffering.

   Additionally required the OpenAL_soft library:
      https://github.com/iLya2IK/libOpenALsoft_dyn
}

program vorbisstreaming;

uses
  {$ifdef LINUX}
  cthreads,
  {$endif}
  Classes, SysUtils,
  OGLVorbisWrapper, OGLOpenALWrapper, OGLOGGWrapper,
  OGLSoundUtils, OGLSoundUtilTypes;

type

  { TOALStreamDataRecorder, TOALStreamDataSource child classes
  to implement vorbis-Ogg data encoding/decoding in streaming mode }

  { TOALVorbisDataRecorder }

  TOALVorbisDataRecorder = class(TOALStreamDataRecorder)
  private
    FStream : TVorbisFile;
  public
    constructor Create(aFormat : TOALFormat; aFreq : Cardinal); override;
    destructor Destroy; override;
    function SaveToFile(const Fn : String) : Boolean; override;
    function SaveToStream(Str : TStream) : Boolean; override;

    procedure StopRecording; override;

    function WriteSamples(const Buffer : Pointer;
                          Count : Integer) : Integer; override;
  end;

  { TOALVorbisDataSource }

  TOALVorbisDataSource = class(TOALStreamDataSource)
  private
    FStream : TVorbisFile;
  public
    constructor Create; override;
    destructor Destroy; override;

    function LoadFromFile(const Fn : String) : Boolean; override;
    function LoadFromStream(Str : TStream) : Boolean; override;

    function ReadChunk(const Buffer : Pointer;
                         Pos : Int64;
                         Sz  : Integer;
                         isloop : Boolean;
                         var fmt : TOALFormat;
                         var freq : Cardinal) : Integer; override;
  end;

constructor TOALVorbisDataSource.Create;
begin
  inherited Create;
  FStream := TVorbisFile.Create;
end;

destructor TOALVorbisDataSource.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TOALVorbisDataSource.LoadFromFile(const Fn : String) : Boolean;
begin
  Result := FStream.LoadFromFile(Fn, false);
end;

function TOALVorbisDataSource.LoadFromStream(Str : TStream) : Boolean;
begin
  Result := false;
end;

function TOALVorbisDataSource.ReadChunk(const Buffer : Pointer; Pos : Int64;
  Sz : Integer; isloop : Boolean; var fmt : TOALFormat; var freq : Cardinal
  ) : Integer;
begin
  fmt    := TOpenAL.OALFormat(FStream.Channels, FStream.Bitdepth);
  freq   := FStream.Frequency;
  Result := FStream.ReadData(Buffer, FStream.Decoder.FrameFromBytes(Sz),
                                     nil).AsBytes;
end;

constructor TOALVorbisDataRecorder.Create(aFormat : TOALFormat; aFreq : Cardinal
  );
begin
  inherited Create(aFormat, aFreq);
  FStream := TVorbisFile.Create;
end;

destructor TOALVorbisDataRecorder.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TOALVorbisDataRecorder.SaveToFile(const Fn : String) : Boolean;
var
  ssize : TSoundSampleSize;
  channels : Cardinal;
begin
  case Format of
  oalfMono8 : begin
    ssize := ss8bit;
    channels := 1;
    end;
  oalfMono16 : begin
    ssize := ss16bit;
    channels := 1;
    end;
  oalfStereo8 : begin
    ssize := ss8bit;
    channels := 2;
    end;
  oalfStereo16 : begin
    ssize := ss16bit;
    channels := 2;
    end;
  end;

  Result := FStream.SaveToFile(Fn,
            TOGLSound.EncProps([TOGLSound.PROP_MODE, oemVBR,
                                TOGLSound.PROP_CHANNELS, channels,
                                TOGLSound.PROP_FREQUENCY, Frequency,
                                TOGLSound.PROP_SAMPLE_SIZE, ssize,
                                TOGLSound.PROP_QUALITY, 0.5,
                                TOGLSound.PROP_BITRATE, 128000]), nil);
end;

function TOALVorbisDataRecorder.SaveToStream(Str : TStream) : Boolean;
begin
  //do nothing
  Result := false;
end;

procedure TOALVorbisDataRecorder.StopRecording;
begin
  FStream.StopStreaming;
end;

function TOALVorbisDataRecorder.WriteSamples(const Buffer : Pointer;
  Count : Integer) : Integer;
begin
  Result := FStream.WriteData(Buffer, FStream.Encoder.FrameFromSamples(Count),
                                      nil).AsSamples;
end;

const // name of file to capture data
      cCaptureFile = 'capture.ogg';
      {$ifdef Windows}
      cOALDLL = '..\libs\soft_oal.dll';
      cOGGDLL = '..\libs\ogg.dll';
      cVorbisDLL : Array [0..2] of String = ('..\libs\vorbis.dll',
                                             '..\libs\vorbisenc.dll',
                                             '..\libs\vorbisfile.dll');
      {$endif}

var
  OALCapture : TOALCapture; // OpenAL audio recoder
  OALPlayer  : TOALPlayer;  // OpenAL audio player
  dt: Integer;
begin
  // Open vorbis, Ogg, OpenAL libraries and initialize interfaces
  {$ifdef Windows}
  if TOpenAL.OALLibsLoad([cOALDLL]) and
     TOGG.OGGLibsLoad([cOGGDLL]) and // nb: vorbis encoder should use
                                     //     ogg library
     TVorbis.VorbisLibsLoad(cVorbisDLL) then
  {$else}
  if TOpenAL.OALLibsLoadDefault and
     TOGG.OGGLibsLoadDefault and TVorbis.VorbisLibsLoadDefault then
  {$endif}
  begin
    // create OpenAL audio recoder
    OALCapture := TOALCapture.Create;
    try
      try
        // config OpenAL audio recoder
        OALCapture.DataRecClass := TOALVorbisDataRecorder;
        // initialize OpenAL audio recoder
        OALCapture.Init;
        // configure buffering for the audio recorder to save data to a file
        if OALCapture.SaveToFile(cCaptureFile) then
        begin
          // start to record data with OpanAL
          OALCapture.Start;

          // run recording loop
          dt := 0;
          while dt < 1000 do begin
            // capture new data chunk and encode/write with vorbisenc to
            // cCaptureFile in vorbis-ogg format
            OALCapture.Proceed;
            TThread.Sleep(10);
            inc(dt);
          end;
          //stop recording. close vorbis-ogg file cCaptureFile
          OALCapture.Stop;

          WriteLn('Capturing completed successfully!');

        end else
          WriteLn('Cant save to ogg-vorbis file');

      except
        on e : Exception do WriteLn(e.ToString);
      end;
    finally
      OALCapture.Free;
    end;

    // create OpenAL audio player
    OALPlayer := TOALPlayer.Create;
    try
      try
        // config OpenAL audio player
        OALPlayer.DataSourceClass := TOALVorbisDataSource;
        // initialize OpenAL audio player
        OALPlayer.Init;
        // configure buffering for the audio player to read data from file
        if OALPlayer.LoadFromFile(cCaptureFile) then
        begin
          // start to play audio data with OpanAL
          OALPlayer.Play;

          // run playing loop. do while the data is available
          while OALPlayer.Status = oalsPlaying do begin
            // if there are empty buffers available - read/decode new data chunk
            // from cCaptureFile with vorbisfile and put them in the queue
            OALPlayer.Stream.Proceed;
            TThread.Sleep(10);
          end;

          WriteLn('Playing completed successfully!');

        end else
          WriteLn('Cant load ogg-vorbis file');

      except
        on e : Exception do WriteLn(e.ToString);
      end;
    finally
      OALPlayer.Free;
    end;

    // close interfaces
    TOpenAL.OALLibsUnLoad;
    TVorbis.VorbisLibsUnLoad;
    TOGG.OGGLibsUnLoad;
  end else
    WriteLn('Cant load libraries');
end.

