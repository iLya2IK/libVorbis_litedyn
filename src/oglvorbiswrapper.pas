{
 OGLVorbisWrapper:
   Wrapper for Vorbis library

   Copyright (c) 2023 by Ilya Medvedkov

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit OGLVorbisWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libVorbis_dyn, OGLOGGWrapper, ctypes;

type

  { IVorbisInfo }

  IVorbisInfo = interface(IUnknown)
  ['{BDCC1BB5-4910-478E-BC00-AE597AD1B869}']

  function Ref : pvorbis_info;

  procedure Init;
  procedure Done;

  function GetBitrateLower : Int64;
  function GetBitrateNominal : Int64;
  function GetBitrateUpper : Int64;
  function GetBitrateWindow : Int64;
  function GetChannels : Integer;
  function GetCodecSetup : pointer;
  function GetRate : Int64;
  function GetVersion : Integer;

  procedure SetBitrateLower(AValue : Int64);
  procedure SetBitrateNominal(AValue : Int64);
  procedure SetBitrateUpper(AValue : Int64);
  procedure SetBitrateWindow(AValue : Int64);
  procedure SetChannels(AValue : Integer);
  procedure SetCodecSetup(AValue : pointer);
  procedure SetRate(AValue : Int64);
  procedure SetVersion(AValue : Integer);

  property Version : Integer read GetVersion write SetVersion;
  property Channels : Integer read GetChannels write SetChannels;
  property Rate : Int64 read GetRate write SetRate;
  property BitrateUpper   : Int64 read GetBitrateUpper write SetBitrateUpper;
  property BitrateNominal : Int64 read GetBitrateNominal write SetBitrateNominal;
  property BitrateLower   : Int64 read GetBitrateLower write SetBitrateLower;
  property BitrateWindow  : Int64 read GetBitrateWindow write SetBitrateWindow;

  property CodecSetup : pointer read GetCodecSetup write SetCodecSetup;
  end;

  { IVorbisBlock }

  IVorbisBlock = interface(IUnknown)
  ['{3D3EAEBF-F44E-494E-B6A2-4082E6EEAA24}']
  function Ref : pvorbis_block;

  procedure Done;

  procedure BitrateAdd;
  function Analysis(OgPac : IOGGPacket) : integer;
  end;

  { IVorbisState }

  IVorbisState = interface(IUnknown)
  ['{65917275-0282-45B7-B66B-423CEDF4A390}']
  function Ref : pvorbis_dsp_state;

  function Init : Boolean;
  procedure Done;

  function Info : IVorbisInfo;
  end;

  { IVorbisEncoder }

  IVorbisEncoder = interface(IVorbisState)
  ['{FEBA3D29-FB43-41B7-925F-83AFF694C9CF}']

  function InitABR(channels: Integer;
            rate, max_bitrate, nominal_bitrate, min_bitrate: Int64) : Boolean;
  function InitVBR(channels: Integer; rate: Int64; base_quality: Single): Boolean;

  function SetupManaged(channels: Integer;
            rate, max_bitrate, nominal_bitrate, min_bitrate: Int64): Boolean;
  function SetupVBR(channels: Integer; rate: Int64; quality: Single): Boolean;
  function SetupInit: Boolean;

  function InitBlock : IVorbisBlock;

  procedure HeaderOut(vc : IOGGComment; header, header_comm, header_code : IOGGPacket);

  function  GetBuffer(BufSize : Integer) : Pointer;
  procedure Wrote(Sz : Integer);

  function BlockOut(b : IVorbisBlock) : Integer;
  function BitrateFlushPacket(OggPac : IOGGPacket) : Integer;

  function Control(Number: Integer; Arg: pointer): Integer;
  end;

  { TVorbisState }

  TVorbisState = class(TInterfacedObject)
  private
    FRef : vorbis_dsp_state;
    FInfo : IVorbisInfo;
    procedure Done;
  public
    constructor Create(inf : IVorbisInfo);
    destructor Destroy; override;

    function Ref : pvorbis_dsp_state; inline;
    function Info : IVorbisInfo; inline;
  end;

  { TVorbisEncoder }

  TVorbisEncoder = class(TVorbisState, IVorbisEncoder)
  private
    function Init : Boolean;
  public
    constructor Create(inf : IVorbisInfo); overload;
    constructor Create(inf : IVorbisInfo;
      channels: Integer; rate: Int64; base_quality: Single); overload;
    constructor Create(inf : IVorbisInfo; channels: Integer;
            rate, max_bitrate, nominal_bitrate, min_bitrate: Int64); overload;

    function InitABR(channels: Integer;
              rate, max_bitrate, nominal_bitrate, min_bitrate: Int64) : Boolean;
    function InitVBR(channels: Integer; rate: Int64; base_quality: Single): Boolean;

    function SetupManaged(channels: Integer;
              rate, max_bitrate, nominal_bitrate, min_bitrate: Int64): Boolean;
    function SetupVBR(channels: Integer; rate: Int64; quality: Single): Boolean;
    function SetupInit: Boolean;

    function InitBlock : IVorbisBlock;
    procedure HeaderOut(vc : IOGGComment; header, header_comm, header_code : IOGGPacket);

    function  GetBuffer(BufSize : Integer) : Pointer;
    procedure Wrote(Sz : Integer);

    function BlockOut(b : IVorbisBlock) : Integer;
    function BitrateFlushPacket(OggPac : IOGGPacket) : Integer;

    function Control(Number: Integer; Arg: pointer): Integer;
  end;

  { TRefVorbisComment }

  TRefVorbisComment = class(TInterfacedObject, IOGGComment)
  private
    FRef : pvorbis_comment;
    procedure Init;
    procedure Done;
  public
    function Ref : Pointer; inline;

    constructor Create(aRef : pvorbis_comment);

    procedure Add(const comment: String);
    procedure AddTag(const tag, value: String);
    function Query(const tag: String; index: integer): String;
    function QueryCount(const tag: String): integer;
  end;

  { TUniqVorbisComment }

  TUniqVorbisComment = class(TRefVorbisComment)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TRefVorbisInfo }

  TRefVorbisInfo = class(TInterfacedObject, IVorbisInfo)
  private
    FPRef : pvorbis_info;
    procedure Init;
    procedure Done;
  public
    function Ref : pvorbis_info; inline;

    constructor Create(aRef : pvorbis_info);

    function GetBitrateLower : Int64;
    function GetBitrateNominal : Int64;
    function GetBitrateUpper : Int64;
    function GetBitrateWindow : Int64;
    function GetChannels : Integer;
    function GetCodecSetup : pointer;
    function GetRate : Int64;
    function GetVersion : Integer;

    procedure SetBitrateLower(AValue : Int64);
    procedure SetBitrateNominal(AValue : Int64);
    procedure SetBitrateUpper(AValue : Int64);
    procedure SetBitrateWindow(AValue : Int64);
    procedure SetChannels(AValue : Integer);
    procedure SetCodecSetup(AValue : pointer);
    procedure SetRate(AValue : Int64);
    procedure SetVersion(AValue : Integer);
  end;

  { TUniqVorbisInfo }

  TUniqVorbisInfo = class(TRefVorbisInfo)
  private
    FRef : vorbis_info;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TVorbisBlock }

  TVorbisBlock = class(TInterfacedObject, IVorbisBlock)
  private
    FRef : vorbis_block;
    procedure Done;
  public
    function Ref : pvorbis_block; inline;

    constructor Create;
    destructor Destroy; override;

    procedure BitrateAdd;
    function  Analysis(OgPac : IOGGPacket) : integer;
  end;

  pOggVorbisEnc = ^OggVorbisEnc;
  OggVorbisEnc = record
    enc   : IVorbisEncoder;
    block : IVorbisBlock;
    info  : IVorbisInfo;
    oggs  : IOGGStreamState;
    bitdepth : Integer;
    quality : Single;
    mode : TOGGSoundEncoderMode;
  end;

  { TVorbisOggEncoder }

  TVorbisOggEncoder = class(TOGGSoundEncoder)
  private
    fRef : pOggVorbisEnc;
    fComm : IOGGComment;
  protected
    procedure Init(aMode : TOGGSoundEncoderMode;
                   aChannels : Cardinal;
                   aFreq, aBitrate, aBitdepth : Cardinal;
                   aQuality : Single;
                   aComments : IOGGComment); override;
    procedure Done; override;

    function GetBitdepth : Cardinal; override;
    function GetBitrate : Cardinal; override;
    function GetChannels : Cardinal; override;
    function GetFrequency : Cardinal; override;
    function GetMode : TOGGSoundEncoderMode; override;
    function GetQuality : Single; override;
    function GetVersion : Integer; override;

    function OggStream : IOGGStreamState; inline;
    function Encoder : IVorbisEncoder; inline;
    procedure FlushOggStream; virtual;
  public
    function Ref : pOggVorbisEnc; inline;

    constructor Create(aMode : TOGGSoundEncoderMode; aChannels : Cardinal; aFreq,
      aBitrate, aBitdepth : Cardinal; aQuality : Single; aComments : IOGGComment);
    destructor Destroy; override;

    function  Comments : IOGGComment; override;

    function  DataMode : TOGGSoundDataMode; override;

    function  WriteData(Buffer : Pointer; Count : Integer; {%H-}Par : Pointer) : Integer; override;
    procedure WriteHeader({%H-}Par : Pointer); override;
    procedure Close({%H-}Par : Pointer); override;

    function Ready : Boolean; override;
  end;

  { TVorbisOggStreamingEncoder }

  TVorbisOggStreamingEncoder = class(TVorbisOggEncoder)
  private
    fStream : TStream;
  protected
    procedure FlushOggStream; override;
  public
    constructor Create(aStream : TStream; aMode : TOGGSoundEncoderMode;
      aChannels : Cardinal; aFreq, aBitrate, aBitdepth : Cardinal;
      aQuality : Single; aComments : IOGGComment);
  end;

  { TVorbisOggDecoder }

  TVorbisOggDecoder = class(TOGGSoundDecoder)
  private
    fRef :  pOggVorbis_File;
    fVorbisInfo : IVorbisInfo;
    fComm : IOGGComment;
    fdec_callbacks : ov_callbacks;

  protected
    procedure Init; override;
    procedure Done; override;

    function GetBitdepth : Cardinal; override;
    function GetBitrate : Cardinal; override;
    function GetChannels : Cardinal; override;
    function GetFrequency : Cardinal; override;
    function GetVersion : Integer; override;
  public
    function Ref : pOggVorbis_File; inline;

    constructor Create;
    destructor Destroy; override;

    function  Comments : IOGGComment; override;

    function  DataMode : TOGGSoundDataMode; override;

    function  ReadData(Buffer : Pointer; Sz : Integer; {%H-}Par : Pointer) : Integer; override;
    procedure ResetToStart; override;

    function Ready : Boolean; override;
  end;

  { TVorbisOggStreamingDecoder }

  TVorbisOggStreamingDecoder = class(TVorbisOggDecoder)
  private
    fStream : TStream;
  protected
    function DoRead(_ptr : Pointer; _nbytes : Integer) : Integer; override;
    function DoSeek(_offset:Int64; _whence:Integer): Integer; override;
    function DoTell:Int64; override;
  public
    constructor Create(aStream : TStream);
  end;

  { TVorbisFile }

  TVorbisFile = class(TOGGSoundFile)
  protected
    function InitEncoder(aMode : TOGGSoundEncoderMode;
                   aChannels : Cardinal;
                   aFreq, aBitrate, aBitdepth : Cardinal;
                   aQuality : Single;
                   aComments : IOGGComment) : TOGGSoundEncoder; override;
    function InitDecoder : TOGGSoundDecoder; override;
  end;

  { TVorbis }

  TVorbis = class
  public
    class function NewComment : IOGGComment;
    class function RefComment(aRef : pvorbis_comment) : IOGGComment;
    class function NewEncoder(inf : IVorbisInfo) : IVorbisEncoder;
    class function NewEncoderVBR(inf : IVorbisInfo;
      channels: Integer; rate: Int64; base_quality: Single) : IVorbisEncoder;
    class function NewEncoderABR(inf : IVorbisInfo; channels: Integer;
            rate, max_bitrate, nominal_bitrate, min_bitrate: Int64) : IVorbisEncoder;
    class function NewInfo : IVorbisInfo;
    class function RefInfo(aRef : pvorbis_info) : IVorbisInfo;

    class function NewStreamingEncoder(aStream : TStream;
                       aMode : TOGGSoundEncoderMode;
                       aChannels : Cardinal;
                       aFreq, aBitrate, aBitdepth : Cardinal;
                       aQuality : Single;
                       aComments : IOGGComment) : TVorbisOggEncoder;
    class function NewStreamingDecoder(aStream : TStream) : TVorbisOggDecoder;

    class procedure UninterleaveSamples(BufW, Buffer : Pointer;
            samples, Channels, BitDepth : integer);

    class function VorbisLibsLoad(const aVorbisLibs : Array of String) : Boolean;
    class function VorbisLibsLoadDefault : Boolean;
    class function IsVorbisLibsLoaded : Boolean;
    class function VorbisLibsUnLoad : Boolean;
  end;

  EVorbis = class(Exception);

implementation

const
  sCantInitEncoder = 'Cant initialize encoder';
  sCantInitAnalysis = 'Cant initialize analysis';
  sVorbisError = 'Vorbis error %d';

function ops_read_func(ptr : pointer; size, nmemb: csize_t; datasource : Pointer): csize_t; cdecl;
begin
  if (size = 0) or (nmemb = 0) then begin Result := 0; Exit; end;
  try
    Result := Int64(TVorbisOggDecoder(datasource).DoRead(ptr, size * nmemb)) div Int64(size);
  except
    Result := 0;
  end;
end;

function ops_seek_func(datasource : pointer; offset: Int64; whence: Integer): Integer; cdecl;
begin
  Result := TVorbisOggDecoder(datasource).DoSeek(offset, whence);
end;

function ops_close_func(datasource : pointer): Integer; cdecl;
begin
  result := 0; 
end;

function ops_tell_func(datasource : pointer): clong; cdecl;
begin
  Result := TVorbisOggDecoder(datasource).DoTell;
end;

{ TUniqVorbisComment }

constructor TUniqVorbisComment.Create;
begin
  Init;
end;

destructor TUniqVorbisComment.Destroy;
begin
  Done;
  inherited Destroy;
end;

{ TVorbisFile }

function TVorbisFile.InitEncoder(aMode : TOGGSoundEncoderMode;
  aChannels : Cardinal; aFreq, aBitrate, aBitdepth : Cardinal;
  aQuality : Single; aComments : IOGGComment) : TOGGSoundEncoder;
begin
  Result := TVorbis.NewStreamingEncoder(Stream, aMode, aChannels, aFreq,
                                            aBitrate,
                                            aBitdepth, aQuality,
                                            aComments);
end;

function TVorbisFile.InitDecoder : TOGGSoundDecoder;
begin
  Result := TVorbis.NewStreamingDecoder(Stream);
end;

{ TVorbisOggDecoder }

procedure TVorbisOggDecoder.Init;
var cError : Integer;
begin
  fdec_callbacks.read_func := @ops_read_func;
  fdec_callbacks.seek_func := @ops_seek_func;
  fdec_callbacks.close_func := @ops_close_func;
  fdec_callbacks.tell_func := @ops_tell_func;

  fRef := GetMem(Sizeof(OggVorbis_File));
  FillByte(fRef^, Sizeof(OggVorbis_File), 0);

  cError := ov_open_callbacks(Pointer(Self), fRef, nil, 0, fdec_callbacks);
  if cError <> 0 then
    raise EVorbis.CreateFmt(sVorbisError, [cError]);

  fComm := TVorbis.RefComment(ov_comment(fRef, -1));

  fVorbisInfo := TVorbis.RefInfo(ov_info(fRef, -1));
end;

procedure TVorbisOggDecoder.Done;
begin
  fVorbisInfo := nil;
  fComm := nil;
  if Assigned(fRef) then
  begin
    if (fRef^.ready_state <> NOTOPEN) then
      ov_clear(fRef);
    FreeMemAndNil(fRef);
  end;
end;

function TVorbisOggDecoder.GetBitdepth : Cardinal;
begin
  Result := 16;
end;

function TVorbisOggDecoder.GetBitrate : Cardinal;
begin
  Result := fVorbisInfo.BitrateNominal;
end;

function TVorbisOggDecoder.GetChannels : Cardinal;
begin
  Result := fVorbisInfo.Channels;
end;

function TVorbisOggDecoder.GetFrequency : Cardinal;
begin
  Result := fVorbisInfo.Rate;
end;

function TVorbisOggDecoder.GetVersion : Integer;
begin
  Result := fVorbisInfo.Version;
end;

function TVorbisOggDecoder.Ref : pOggVorbis_File;
begin
  Result := fRef;
end;

constructor TVorbisOggDecoder.Create;
begin
  Init;
end;

destructor TVorbisOggDecoder.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TVorbisOggDecoder.Comments : IOGGComment;
begin
  Result := FComm;
end;

function TVorbisOggDecoder.DataMode : TOGGSoundDataMode;
begin
  Result := odmBytes;
end;

function TVorbisOggDecoder.ReadData(Buffer : Pointer; Sz : Integer;
  Par : Pointer) : Integer;
var
  Size, Section, Res: Integer;
begin
  Size := 0;
  while (Size < Sz) do begin
    Res := ov_read(fRef, @(PByte(Buffer)[Size]), Sz - Size, 0, 2, 1, @Section);
    if Res > 0 then inc(Size, Res) else break;
  end;
  Result := Size;
end;

procedure TVorbisOggDecoder.ResetToStart;
begin
  ov_pcm_seek(fRef, 0);
end;

function TVorbisOggDecoder.Ready : Boolean;
begin
  result := Assigned(fRef);
end;

{ TVorbisOggStreamingDecoder }

function TVorbisOggStreamingDecoder.DoRead(_ptr : Pointer; _nbytes : Integer
  ) : Integer;
begin
  Result := fStream.Read(_ptr^, _nbytes);
end;

function TVorbisOggStreamingDecoder.DoSeek(_offset : Int64; _whence : Integer
  ) : Integer;
begin
  try
    with fStream do
      case _whence of
        0: Seek(_offset, soBeginning);
        1: Seek(_offset, soCurrent);
        2: Seek(_offset, soEnd);
      end;
    result := 0;
  except
    result := -1;
  end;
end;

function TVorbisOggStreamingDecoder.DoTell : Int64;
begin
  try
    result := fStream.Position;
  except
    result := -1;
  end;
end;

constructor TVorbisOggStreamingDecoder.Create(aStream : TStream);
begin
  fStream := aStream;
  inherited Create;
end;

{ TVorbisOggStreamingEncoder }

procedure TVorbisOggStreamingEncoder.FlushOggStream;
begin
  OggStream.PagesOutToStream(fStream);
end;

constructor TVorbisOggStreamingEncoder.Create(aStream : TStream;
  aMode : TOGGSoundEncoderMode; aChannels : Cardinal; aFreq,
  aBitrate, aBitdepth : Cardinal; aQuality : Single;
  aComments : IOGGComment);
begin
  fStream := aStream;
  inherited Create(aMode, aChannels, aFreq, aBitrate, aBitdepth, aQuality, aComments);
end;

{ TVorbisOggEncoder }

procedure TVorbisOggEncoder.Init(aMode : TOGGSoundEncoderMode;
  aChannels : Cardinal; aFreq, aBitrate, aBitdepth : Cardinal;
  aQuality : Single; aComments : IOGGComment);
begin
  fRef := GetMem(Sizeof(OggVorbisEnc));
  FillByte(fRef^, Sizeof(OggVorbisEnc), 0);

  fRef^.oggs := TOGG.NewStream(Abs(Random(Int64(Now))));
  if Assigned(aComments) then
  begin
    fComm := aComments;
  end else
    FComm := TVorbis.NewComment;
  fRef^.info := TVorbis.NewInfo;

  if aMode = oemVBR then
  begin
    fRef^.enc := TVorbis.NewEncoderVBR(fRef^.info, aChannels,
                                            aFreq, aQuality);
  end else
  if aMode = oemCBR then
  begin
    fRef^.enc := TVorbis.NewEncoderABR(fRef^.info, aChannels,
                                            aFreq, -1, aBitrate * 1000, -1);
  end;
  fRef^.block := Encoder.InitBlock;
  fRef^.bitdepth := aBitdepth;
  fRef^.mode := aMode;
end;

procedure TVorbisOggEncoder.Done;
begin
  if Assigned(fRef) then
  begin
    fRef^.block := nil;
    fRef^.enc := nil;
    fRef^.info := nil;
    FreeMemAndNil(fRef);
  end;
  fComm := nil;
end;

function TVorbisOggEncoder.GetBitdepth : Cardinal;
begin
  Result := fRef^.bitdepth;
end;

function TVorbisOggEncoder.GetBitrate : Cardinal;
begin
  Result := fRef^.info.BitrateNominal;
end;

function TVorbisOggEncoder.GetChannels : Cardinal;
begin
  Result := fRef^.info.Channels;
end;

function TVorbisOggEncoder.GetFrequency : Cardinal;
begin
  Result := fRef^.info.Rate;
end;

function TVorbisOggEncoder.GetMode : TOGGSoundEncoderMode;
begin
  Result := fRef^.mode;
end;

function TVorbisOggEncoder.GetQuality : Single;
begin
  Result := 0.0;
end;

function TVorbisOggEncoder.GetVersion : Integer;
begin
  Result := fRef^.info.Version;
end;

function TVorbisOggEncoder.OggStream : IOGGStreamState;
begin
  Result := fRef^.oggs;
end;

function TVorbisOggEncoder.Encoder : IVorbisEncoder;
begin
  Result := fRef^.enc;
end;

procedure TVorbisOggEncoder.FlushOggStream;
begin
  // do nothing
end;

function TVorbisOggEncoder.Ref : pOggVorbisEnc;
begin
  Result := fRef
end;

constructor TVorbisOggEncoder.Create(aMode : TOGGSoundEncoderMode;
  aChannels : Cardinal; aFreq, aBitrate, aBitdepth : Cardinal; aQuality : Single;
  aComments : IOGGComment);
begin
  Init(aMode, aChannels, aFreq, aBitrate, aBitdepth, aQuality,
              aComments);
end;

destructor TVorbisOggEncoder.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TVorbisOggEncoder.Comments : IOGGComment;
begin
  Result := fComm;
end;

function TVorbisOggEncoder.DataMode : TOGGSoundDataMode;
begin
  Result := odmSamples;
end;

function TVorbisOggEncoder.WriteData(Buffer : Pointer; Count : Integer;
  Par : Pointer) : Integer;
var
  BufW : Pointer;
  og : IOGGPacket;
begin
  if Count > 0 then
  begin
    BufW := Encoder.GetBuffer(Count);
    TVorbis.UninterleaveSamples(BufW, Buffer, Count, GetChannels, GetBitDepth);
    Encoder.Wrote(Count);

    og := TOgg.NewPacket;

    while (Encoder.BlockOut(fRef^.block) = 1) do
    begin
      fRef^.block.Analysis(nil);
      fRef^.block.BitrateAdd;

      while (Encoder.BitrateFlushPacket(og) <> 0) do
      begin
        OggStream.PacketIn(og);
        FlushOggStream;
      end;
    end;
    Result := Count;
  end else
    Result := 0;
end;

procedure TVorbisOggEncoder.WriteHeader(Par : Pointer);
var
  header,
  header_comm,
  header_code : IOGGPacket;
begin
  header := TOgg.NewPacket;
  header_comm := TOgg.NewPacket;
  header_code := TOgg.NewPacket;

  Encoder.HeaderOut(fComm,header,header_comm,header_code);

  OggStream.PacketIn(header);
  OggStream.PacketIn(header_comm);
  OggStream.PacketIn(header_code);

  FlushOggStream;
end;

procedure TVorbisOggEncoder.Close(Par : Pointer);
var
  og : IOGGPacket;
begin
  Encoder.Wrote(0);

  og := TOgg.NewPacket;

  while (Encoder.BlockOut(fRef^.block) = 1) do
  begin
    fRef^.block.Analysis(nil);
    fRef^.block.BitrateAdd;

    while (Encoder.BitrateFlushPacket(og) <> 0) do
    begin
      OggStream.PacketIn(og);
      FlushOggStream;
    end;
  end;
end;

function TVorbisOggEncoder.Ready : Boolean;
begin
  Result := Assigned(fRef) and Assigned(fRef^.enc);
end;

{ TUniqVorbisInfo }

constructor TUniqVorbisInfo.Create;
begin
  FillByte(Fref, sizeof(FRef), 0);
  inherited Create(@FRef);
  Init;
end;

destructor TUniqVorbisInfo.Destroy;
begin
  Done;
  inherited Destroy;
end;

{ TVorbisState }

procedure TVorbisState.Done;
begin
  vorbis_dsp_clear(@FRef);
end;

constructor TVorbisState.Create(inf : IVorbisInfo);
begin
  FInfo := inf;
end;

destructor TVorbisState.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TVorbisState.Ref : pvorbis_dsp_state;
begin
  Result := @FRef;
end;

function TVorbisState.Info : IVorbisInfo;
begin
  Result := FInfo;
end;

{ TVorbisEncoder }

function TVorbisEncoder.Init : Boolean;
begin
  Result := vorbis_analysis_init(@FRef, Info.Ref) = 0;
end;

constructor TVorbisEncoder.Create(inf : IVorbisInfo);
begin
  inherited Create(inf);
  if not InitVBR(2, 48000, 0.5) then
    raise EVorbis.Create(sCantInitEncoder);
  if not Init then
    raise EVorbis.Create(sCantInitAnalysis);
end;

constructor TVorbisEncoder.Create(inf : IVorbisInfo; channels : Integer;
  rate : Int64; base_quality : Single);
begin
  inherited Create(inf);
  if not InitVBR(channels, rate, base_quality) then
    raise EVorbis.Create(sCantInitEncoder);
  if not Init then
    raise EVorbis.Create(sCantInitAnalysis);
end;

constructor TVorbisEncoder.Create(inf : IVorbisInfo; channels : Integer; rate,
  max_bitrate, nominal_bitrate, min_bitrate : Int64);
begin
  inherited Create(inf);
  if not InitABR(channels, rate, max_bitrate, nominal_bitrate, min_bitrate) then
    raise EVorbis.Create(sCantInitEncoder);
  if not Init then
    raise EVorbis.Create(sCantInitAnalysis);
end;

function TVorbisEncoder.InitABR(channels : Integer; rate, max_bitrate,
  nominal_bitrate, min_bitrate : Int64) : Boolean;
begin
  Result := vorbis_encode_init(Info.Ref, channels, rate, max_bitrate, nominal_bitrate, min_bitrate) = 0;
end;

function TVorbisEncoder.InitVBR(channels : Integer; rate : Int64;
  base_quality : Single) : Boolean;
begin
  Result := vorbis_encode_init_vbr(Info.Ref, channels, rate, base_quality) = 0;
end;

function TVorbisEncoder.SetupManaged(channels : Integer; rate, max_bitrate,
  nominal_bitrate, min_bitrate : Int64) : Boolean;
begin
  Result := vorbis_encode_setup_managed(Info.Ref, channels, rate, max_bitrate,
                                        nominal_bitrate, min_bitrate) = 0;
end;

function TVorbisEncoder.SetupVBR(channels : Integer; rate : Int64;
  quality : Single) : Boolean;
begin
  Result := vorbis_encode_setup_vbr(Info.Ref, channels, rate, quality) = 0;
end;

function TVorbisEncoder.SetupInit : Boolean;
begin
  Result := vorbis_encode_setup_init(Info.Ref) = 0;
end;

function TVorbisEncoder.InitBlock : IVorbisBlock;
begin
  Result := TVorbisBlock.Create as IVorbisBlock;
  vorbis_block_init(Ref, Result.Ref);
end;

procedure TVorbisEncoder.HeaderOut(vc : IOGGComment; header, header_comm,
  header_code : IOGGPacket);
begin
  vorbis_analysis_headerout(Ref,vc.Ref,header.Ref,header_comm.Ref,header_code.Ref);
end;

function TVorbisEncoder.GetBuffer(BufSize : Integer) : Pointer;
begin
  Result := vorbis_analysis_buffer(@FRef, BufSize);
end;

procedure TVorbisEncoder.Wrote(Sz : Integer);
begin
  vorbis_analysis_wrote(@FRef, Sz);
end;

function TVorbisEncoder.BlockOut(b : IVorbisBlock) : Integer;
begin
  Result := vorbis_analysis_blockout(@FRef, b.Ref);
end;

function TVorbisEncoder.BitrateFlushPacket(OggPac : IOGGPacket) : Integer;
begin
  Result := vorbis_bitrate_flushpacket(@FRef, OggPac.Ref);
end;

function TVorbisEncoder.Control(Number : Integer; Arg : pointer) : Integer;
begin
  Result := vorbis_encode_ctl(Info.Ref, Number, Arg);
end;

{ TVorbisBlock }

procedure TVorbisBlock.Done;
begin
  vorbis_block_clear(@FRef);
end;

function TVorbisBlock.Ref : pvorbis_block;
begin
  Result := @FRef
end;

constructor TVorbisBlock.Create;
begin
  FillByte(FRef, Sizeof(FRef), 0);
end;

destructor TVorbisBlock.Destroy;
begin
  Done;
  inherited Destroy;
end;

procedure TVorbisBlock.BitrateAdd;
begin
  vorbis_bitrate_addblock(@FRef);
end;

function TVorbisBlock.Analysis(OgPac : IOGGPacket) : integer;
begin
  if Assigned(OgPac) then
    Result := vorbis_analysis(@FRef, OgPac.Ref) else
    Result := vorbis_analysis(@FRef, nil);
end;

{ TRefVorbisInfo }

procedure TRefVorbisInfo.Init;
begin
  vorbis_info_init(FPRef);
end;

procedure TRefVorbisInfo.Done;
begin
  vorbis_info_clear(FPRef);
end;

function TRefVorbisInfo.Ref : pvorbis_info;
begin
  Result := FPRef
end;

constructor TRefVorbisInfo.Create(aRef : pvorbis_info);
begin
  FPRef := aRef;
end;

function TRefVorbisInfo.GetBitrateLower : Int64;
begin
  Result := FPRef^.bitrate_lower;
end;

function TRefVorbisInfo.GetBitrateNominal : Int64;
begin
  Result := FPRef^.bitrate_nominal;
end;

function TRefVorbisInfo.GetBitrateUpper : Int64;
begin
  Result := FPRef^.bitrate_upper;
end;

function TRefVorbisInfo.GetBitrateWindow : Int64;
begin
  Result := FPRef^.bitrate_window;
end;

function TRefVorbisInfo.GetChannels : Integer;
begin
  Result := FPRef^.channels;
end;

function TRefVorbisInfo.GetCodecSetup : pointer;
begin
  Result := FPRef^.codec_setup;
end;

function TRefVorbisInfo.GetRate : Int64;
begin
  Result := FPRef^.rate;
end;

function TRefVorbisInfo.GetVersion : Integer;
begin
  Result := FPRef^.version;
end;

procedure TRefVorbisInfo.SetBitrateLower(AValue : Int64);
begin
  FPRef^.bitrate_lower := AValue;
end;

procedure TRefVorbisInfo.SetBitrateNominal(AValue : Int64);
begin
  FPRef^.bitrate_nominal := AValue;
end;

procedure TRefVorbisInfo.SetBitrateUpper(AValue : Int64);
begin
  FPRef^.bitrate_upper := AValue;
end;

procedure TRefVorbisInfo.SetBitrateWindow(AValue : Int64);
begin
  FPRef^.bitrate_window := AValue;
end;

procedure TRefVorbisInfo.SetChannels(AValue : Integer);
begin
  FPRef^.channels := AValue;
end;

procedure TRefVorbisInfo.SetCodecSetup(AValue : pointer);
begin
  FPRef^.codec_setup := AValue;
end;

procedure TRefVorbisInfo.SetRate(AValue : Int64);
begin
  FPRef^.rate := AValue;
end;

procedure TRefVorbisInfo.SetVersion(AValue : Integer);
begin
  FPRef^.version := AValue;
end;

{ TRefVorbisComment }

procedure TRefVorbisComment.Init;
begin
  FRef := GetMem(Sizeof(vorbis_comment));
  FillByte(fRef^, Sizeof(vorbis_comment), 0);
  vorbis_comment_init(FRef);
end;

procedure TRefVorbisComment.Done;
begin
  if Assigned(FRef) then
  begin
    vorbis_comment_clear(FRef);
    FreeMemAndNil(FRef);
  end;
end;

function TRefVorbisComment.Ref : Pointer;
begin
  Result := FRef;
end;

constructor TRefVorbisComment.Create(aRef : pvorbis_comment);
begin
  fRef := aRef;
end;

procedure TRefVorbisComment.Add(const comment : String);
begin
  vorbis_comment_add(FRef, pcchar(pchar(comment)));
end;

procedure TRefVorbisComment.AddTag(const tag, value : String);
begin
  vorbis_comment_add_tag(@FRef, pcchar(pchar(tag)), pcchar(pchar(value)));
end;

function TRefVorbisComment.Query(const tag : String; index : integer) : String;
begin
  Result := StrPas( PChar(vorbis_comment_query(@FRef, pcchar(PChar(tag)), index)) );
end;

function TRefVorbisComment.QueryCount(const tag : String) : integer;
begin
  Result := vorbis_comment_query_count(@FRef, pcchar(PChar(tag)));
end;

{ TVorbis }

class function TVorbis.NewComment : IOGGComment;
begin
  Result := TUniqVorbisComment.Create as IOGGComment;
end;

class function TVorbis.RefComment(aRef : pvorbis_comment) : IOGGComment;
begin
  Result := TRefVorbisComment.Create(aRef) as IOGGComment;
end;

class function TVorbis.NewEncoder(inf : IVorbisInfo) : IVorbisEncoder;
begin
  Result := TVorbisEncoder.Create(inf) as IVorbisEncoder;
end;

class function TVorbis.NewEncoderVBR(inf : IVorbisInfo; channels : Integer;
  rate : Int64; base_quality : Single) : IVorbisEncoder;
begin
  Result := TVorbisEncoder.Create(inf, channels, rate, base_quality) as IVorbisEncoder;
end;

class function TVorbis.NewEncoderABR(inf : IVorbisInfo; channels : Integer;
  rate, max_bitrate, nominal_bitrate, min_bitrate : Int64) : IVorbisEncoder;
begin
  Result := TVorbisEncoder.Create(inf, channels,
                                       rate, max_bitrate,
                                       nominal_bitrate,
                                       min_bitrate) as IVorbisEncoder;
end;

class function TVorbis.NewInfo : IVorbisInfo;
begin
  Result := TUniqVorbisInfo.Create as IVorbisInfo;
end;

class function TVorbis.RefInfo(aRef : pvorbis_info) : IVorbisInfo;
begin
  Result := TRefVorbisInfo.Create(aRef) as IVorbisInfo;
end;

class function TVorbis.NewStreamingEncoder(aStream : TStream;
  aMode : TOGGSoundEncoderMode; aChannels : Cardinal; aFreq,
  aBitrate, aBitdepth : Cardinal; aQuality : Single; aComments : IOGGComment
  ) : TVorbisOggEncoder;
begin
  Result := TVorbisOggStreamingEncoder.Create(aStream, aMode, aChannels,
                                            aFreq,
                                            aBitrate,
                                            aBitdepth,
                                            aQuality,
                                            aComments);
end;

class function TVorbis.NewStreamingDecoder(aStream : TStream
  ) : TVorbisOggDecoder;
begin
  Result := TVorbisOggStreamingDecoder.Create(aStream);
end;

class procedure TVorbis.UninterleaveSamples(BufW, Buffer : Pointer; samples,
  Channels, BitDepth : integer);
var i : integer;
    fb : PPointer absolute BufW;
begin
  if BitDepth <> 16 then Exit;
  if Channels = 1 then
  begin
    for i:=0 to samples-1 do
    begin
      PSingle(fb[0])[i]:=Single(PSmallInt(Buffer)[i]) / 32768.0;
    end;
  end else
  if Channels = 2 then
  begin
    for i:=0 to samples-1 do
    begin
        PSingle(fb[0])[i]:=Single(PSmallInt(Buffer)[i*2])   / 32768.0;
        PSingle(fb[1])[i]:=Single(PSmallInt(Buffer)[i*2+1]) / 32768.0;
    end;
  end;
end;

class function TVorbis.VorbisLibsLoad(const aVorbisLibs : array of String
  ) : Boolean;
begin
  Result := InitVorbisInterface(aVorbisLibs);
end;

class function TVorbis.VorbisLibsLoadDefault : Boolean;
begin
  Result := InitVorbisInterface(VorbisDLL);
end;

class function TVorbis.IsVorbisLibsLoaded : Boolean;
begin
  Result := IsVorbisloaded;
end;

class function TVorbis.VorbisLibsUnLoad : Boolean;
begin
  Result := DestroyVorbisInterface;
end;

end.

