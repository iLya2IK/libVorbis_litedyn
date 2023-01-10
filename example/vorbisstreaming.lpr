program vorbisstreaming;

uses
  {$ifdef LINUX}
  cthreads,
  {$endif}
  Classes, SysUtils,
  OGLVorbisWrapper, OGLOpenALWrapper, OGLOGGWrapper;

type

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
  Result := FStream.ReadData(Buffer, Sz, nil);
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
  bits, channels : Cardinal;
begin
  case Format of
  oalfMono8 : begin
    bits := 8;
    channels := 1;
    end;
  oalfMono16 : begin
    bits := 16;
    channels := 1;
    end;
  oalfStereo8 : begin
    bits := 8;
    channels := 2;
    end;
  oalfStereo16 : begin
    bits := 16;
    channels := 2;
    end;
  end;

  Result := FStream.SaveToFile(Fn, oemVBR, channels, Frequency, 128, bits, 0.5, nil);
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
  Result := FStream.WriteSamples(Buffer, Count, nil);
end;

const cCaptureFile = 'capture.ogg';
      {$ifdef Windows}
      cOALDLL = '..\libs\soft_oal.dll';
      cOGGDLL = '..\libs\ogg.dll';
      cVorbisDLL : Array [0..2] of String = ('..\libs\vorbis.dll',
                                             '..\libs\vorbisenc.dll',
                                             '..\libs\vorbisfile.dll');
      {$endif}

var
  OALCapture : TOALCapture;
  OALPlayer  : TOALPlayer;
  dt: Integer;
begin
  {$ifdef Windows}
  if TOpenAL.OALLibsLoad([cOALDLL]) and
     TOGG.OGGLibsLoad([cOGGDLL]) and
     TVorbis.VorbisLibsLoad(cVorbisDLL) then
  {$else}
  if TOpenAL.OALLibsLoadDefault and
     TOGG.OGGLibsLoadDefault and TVorbis.VorbisLibsLoadDefault then
  {$endif}
  begin
    OALCapture := TOALCapture.Create;
    try
      try
        OALCapture.DataRecClass := TOALVorbisDataRecorder;
        OALCapture.Init;
        if OALCapture.SaveToFile(cCaptureFile) then
        begin
          OALCapture.Start;

          dt := 0;
          while dt < 1000 do begin
            OALCapture.Proceed;
            TThread.Sleep(10);
            inc(dt);
          end;

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

    OALPlayer := TOALPlayer.Create;
    try
      try
        OALPlayer.DataSourceClass := TOALVorbisDataSource;
        OALPlayer.Init;
        if OALPlayer.LoadFromFile(cCaptureFile) then
        begin

          OALPlayer.Play;

          while OALPlayer.Status = oalsPlaying do begin
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

    TOpenAL.OALLibsUnLoad;
    TVorbis.VorbisLibsUnLoad;
    TOGG.OGGLibsUnLoad;
  end else
    WriteLn('Cant load libraries');
  ReadLn;
end.

