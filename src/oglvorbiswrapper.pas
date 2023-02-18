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
  Classes, SysUtils, libVorbis_dyn, OGLOGGWrapper,
  OGLSoundUtils, OGLSoundUtilTypes, OGLSoundDataConverting,
  ctypes;

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
  function Synthesis(OgPac: IOGGPacket) : integer;
  function TrackOnly(OgPac: IOGGPacket) : Integer;
  end;

  { IVorbisState }

  IVorbisState = interface(IUnknown)
  ['{65917275-0282-45B7-B66B-423CEDF4A390}']
  function Ref : pvorbis_dsp_state;

  function Init : Boolean;
  procedure Done;

  function InitBlock : IVorbisBlock;

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

  procedure HeaderOut(vc : ISoundComment; header, header_comm, header_code : IOGGPacket);

  function  GetBuffer(BufSize : Integer) : Pointer;
  procedure Wrote(Sz : Integer);

  function BlockOut(b : IVorbisBlock) : Integer;
  function BitrateFlushPacket(OggPac : IOGGPacket) : Integer;

  function Control(Number: Integer; Arg: pointer): Integer;
  end;

  { IVorbisDecoder }

  IVorbisDecoder = interface(IVorbisState)
  ['{6BAAAF6B-4353-424F-A89A-EA0C283326E0}']

  function IDHeader(op: IOGGPacket): integer;
  function HeaderIn(vc: ISoundComment; op: IOGGPacket): integer;
  function Restart : Integer;

  function BlockIn(b : IVorbisBlock) : Integer;

  function PCMOut(pcm: pointer): Integer;
  function LapOut(pcm: pointer): Integer;
  function Read(samples: Integer): Integer;
  function BlockSize(op: IOGGPacket): Longint;
  function Halfrate(flag : Boolean) : Integer;
  function IsHalfrate : Boolean;
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

    function InitBlock : IVorbisBlock;

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

    procedure HeaderOut(vc : ISoundComment; header, header_comm, header_code : IOGGPacket);

    function  GetBuffer(BufSize : Integer) : Pointer;
    procedure Wrote(Sz : Integer);

    function BlockOut(b : IVorbisBlock) : Integer;
    function BitrateFlushPacket(OggPac : IOGGPacket) : Integer;

    function Control(Number: Integer; Arg: pointer): Integer;
  end;

  { TVorbisDecoder }

  TVorbisDecoder = class(TVorbisState, IVorbisDecoder)
  private
    function Init : Boolean;
  public
    constructor Create(inf : IVorbisInfo); overload;
    constructor CreateNotInit(inf : IVorbisInfo);

    function IDHeader(op: IOGGPacket): Integer;
    function HeaderIn(vc: ISoundComment; op: IOGGPacket): Integer;
    function Restart : Integer;

    function BlockIn(b : IVorbisBlock) : Integer;

    function PCMOut(pcm: pointer): Integer;
    function LapOut(pcm: pointer): Integer;
    function Read(samples: Integer): Integer;
    function BlockSize(op: IOGGPacket): Longint;
    function Halfrate(flag : Boolean) : Integer;
    function IsHalfrate : Boolean;
  end;

  { TRefVorbisComment }

  TRefVorbisComment = class(TNativeVorbisCommentCloneable)
  private
    FRef : pvorbis_comment;
  protected
    procedure Init; override;
    procedure Done; override;

    procedure SetVendor(const {%H-}S : String); override;
    procedure SetNativeVendor({%H-}v : PChar); override;
    function GetNativeVendor : PChar; override;
    function GetNativeComment(index : integer) : PChar; override;
    function GetNativeCommentLength(index : integer) : Int32; override;
    function GetNativeCommentCount : Int32; override;
  public
    function Ref : Pointer; override;

    constructor Create(aRef : pvorbis_comment); overload;

    procedure Add(const comment: String); override;
    procedure AddTag(const tag, value: String); override;
    function Query(const tag: String; index: Integer): String; override;
    function QueryCount(const tag: String): Integer; override;
  end;

  { TUniqVorbisComment }

  TUniqVorbisComment = class(TRefVorbisComment)
  public
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
    function  Analysis(OgPac : IOGGPacket) : Integer;
    function  Synthesis(OgPac: IOGGPacket) : Integer;
    function  TrackOnly(OgPac: IOGGPacket) : Integer;
  end;

  pOggVorbisEnc = ^OggVorbisEnc;
  OggVorbisEnc = record
    enc   : IVorbisEncoder;
    block : IVorbisBlock;
    info  : IVorbisInfo;
    oggs  : IOGGStreamState;
    ssize : TSoundSampleSize;
    quality : Single;
    mode : TSoundEncoderMode;
  end;

  { TVorbisOggEncoder }

  TVorbisOggEncoder = class(TSoundAbstractEncoder)
  private
    fRef : pOggVorbisEnc;
    fComm : ISoundComment;
  protected
    procedure Init(aProps : ISoundEncoderProps;
                   aComments : ISoundComment); override;
    procedure Done; override;

    function GetSampleSize : TSoundSampleSize; override;
    function GetBitrate : Cardinal; override;
    function GetChannels : Cardinal; override;
    function GetFrequency : Cardinal; override;
    function GetMode : TSoundEncoderMode; override;
    function GetQuality : Single; override;
    function GetVersion : Integer; override;

    function OggStream : IOGGStreamState; inline;
    function Encoder : IVorbisEncoder; inline;
    procedure WriteStreamPages(aFlush : Boolean);
    procedure WriteOggStream({%H-}aFlush : Boolean); virtual;
  public
    function Ref : pOggVorbisEnc; inline;

    constructor Create(aProps : ISoundEncoderProps; aComments : ISoundComment);
    destructor Destroy; override;

    function  Comments : ISoundComment; override;

    function  WriteData(Buffer : Pointer; Count : ISoundFrameSize;
                               {%H-}Par : Pointer) : ISoundFrameSize; override;
    procedure WriteHeader({%H-}Par : Pointer); override;
    procedure Close({%H-}Par : Pointer); override;
    procedure Flush({%H-}Par : Pointer); override;

    function Ready : Boolean; override;
  end;

  { TVorbisOggStreamEncoder }

  TVorbisOggStreamEncoder = class(TVorbisOggEncoder, ISoundStreamEncoder)
  protected
    procedure WriteOggStream(aFlush : Boolean); override;
  public
    constructor Create(aStream : TStream; aDataLimits : TSoundDataLimits;
                       aProps : ISoundEncoderProps;
                       aComments : ISoundComment);
    procedure SetStream(aStream : TStream); virtual;
    procedure ReInitEncoder;
  end;

  pOggVorbisFileDec = ^OggVorbisFileDec;
  OggVorbisFileDec = record
    ovfile    : OggVorbis_File;
    info      : IVorbisInfo;
    callbacks : ov_callbacks;
  end;

  { TVorbisOggDecoder }

  TVorbisOggDecoder = class(TSoundAbstractDecoder)
  private
    fRef  : Pointer;
    fComm : ISoundComment;

  protected
    function GetSampleSize : TSoundSampleSize; override;
    function GetBitrate : Cardinal; override;
    function GetChannels : Cardinal; override;
    function GetFrequency : Cardinal; override;
    function GetVersion : Integer; override;

    function VorbisInfo : IVorbisInfo; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function  Comments : ISoundComment; override;

    function Ready : Boolean; override;
  end;

  { TVorbisOggFileDecoder }

  TVorbisOggFileDecoder = class(TVorbisOggDecoder)
  protected
    procedure Init; override;
    procedure Done; override;

    function VorbisInfo : IVorbisInfo; override;
    function OVFile : pOggVorbis_File; inline;
  public
    function Ref : pOggVorbisFileDec; inline;

    function  ReadData(Buffer : Pointer; Sz : ISoundFrameSize;
                              {%H-}Par : Pointer) : ISoundFrameSize; override;
    procedure ResetToStart; override;
    procedure RawSeek(pos : Int64); override;
    procedure SampleSeek(pos : Integer); override;
    procedure TimeSeek(pos : Double); override;
    function RawTell : Int64; override;
    function SampleTell : Integer; override;
    function TimeTell : Double; override;
    function RawTotal : Int64; override;
    function SampleTotal : Integer; override;
    function TimeTotal : Double; override;
  end;

  { not needed vorbisfile library }

  pOggVorbisDec = ^OggVorbisDec;
  OggVorbisDec = record
    dec   : IVorbisDecoder;
    block : IVorbisBlock;
    osyn  : IOGGSyncState;
    ostr  : IOGGStreamState;
    page  : IOGGPage;
    pack  : IOGGPacket;
    pcm_buffer : Pointer;
    state : Byte;
  end;

  { TVorbisOggAltDecoder }

  TVorbisOggAltDecoder = class(TVorbisOggDecoder)
  private
    function ReadDataToOgg : Integer;
    procedure ReadHeaders;
  protected
    procedure Init; override;
    procedure Done; override;

    function VorbisInfo : IVorbisInfo; override;

    function OggState : IOGGSyncState; inline;
    function OggStream : IOGGStreamState; inline;
    function OggPage : IOGGPage; inline;
    function OggPacket : IOGGPacket; inline;
    function Decoder : IVorbisDecoder; inline;

    class function DecoderReadBufferSize : Integer; virtual;
  public
    function Ref : pOggVorbisDec; inline;

    function  ReadData(Buffer : Pointer; Sz : ISoundFrameSize;
                              {%H-}Par : Pointer) : ISoundFrameSize; override;
    procedure ResetToStart; override;
  end;

  { TVorbisOggStreamFileDecoder }

  TVorbisOggStreamFileDecoder = class(TVorbisOggFileDecoder, ISoundStreamDecoder)
  public
    constructor Create(aStream : TStream; aDataLimits : TSoundDataLimits);
    procedure SetStream(aStream : TStream); virtual;
  end;

  { TVorbisOggStreamAltDecoder }

  TVorbisOggStreamAltDecoder = class(TVorbisOggAltDecoder, ISoundStreamDecoder)
  public
    constructor Create(aStream : TStream);
    procedure SetStream(aStream : TStream); virtual;
  end;

  { TVorbisFile }

  TVorbisFile = class(TSoundFile)
  protected
    function InitEncoder(aProps : ISoundEncoderProps;
                         aComments : ISoundComment) : ISoundEncoder; override;
    function InitDecoder : ISoundDecoder; override;
  end;

  { TVorbisFileAlt }

  TVorbisFileAlt = class(TVorbisFile)
  protected
    function InitDecoder : ISoundDecoder; override;
  end;

  { TVorbis }

  TVorbis = class
  public
    class function NewComment : ISoundComment;
    class function NewComment(src : ISoundComment) : ISoundComment;
    class function RefComment(aRef : pvorbis_comment) : ISoundComment;
    class function NewEncoder(inf : IVorbisInfo) : IVorbisEncoder;
    class function NewEncoderVBR(inf : IVorbisInfo;
      channels: Integer; rate: Int64; base_quality: Single) : IVorbisEncoder;
    class function NewEncoderABR(inf : IVorbisInfo; channels: Integer;
            rate, max_bitrate, nominal_bitrate, min_bitrate: Int64) : IVorbisEncoder;
    class function NewInfo : IVorbisInfo;
    class function RefInfo(aRef : pvorbis_info) : IVorbisInfo;
    class function NewDecoder(inf : IVorbisInfo) : IVorbisDecoder;

    class function NewOggStreamEncoder(aStream : TStream;
                                       aDataLimits : TSoundDataLimits;
                                       aProps : ISoundEncoderProps;
                                       aComments : ISoundComment) : ISoundStreamEncoder;
    class function NewOggStreamFileDecoder(aStream : TStream;
                                           aDataLimits : TSoundDataLimits) : ISoundStreamDecoder;
    class function NewOggStreamAltDecoder(aStream : TStream) : ISoundStreamDecoder;

    class function EncoderVersionString : String;

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
  sCantInitSynthesis = 'Cant initialize synthesis';
  sWrongOggStream = 'Input does not appear to be an Ogg bitstream';
  sWrongHeader = 'Error reading initial header packet';
  sNotVorbis = 'This Ogg bitstream does not contain Vorbis audio data';
  sCorruptSecondaryHeader = 'Corrupt secondary header';
  sUnexpectedEndOfFile = 'End of file before finding all Vorbis headers';
  sVorbisError = 'Vorbis error %d';

function ops_read_func(ptr : pointer; size, nmemb: csize_t; datasource : Pointer): csize_t; cdecl;
begin
  if (size = 0) or (nmemb = 0) then begin Result := 0; Exit; end;
  try
    Result := Int64(TVorbisOggDecoder(datasource).DataStream.DoRead(ptr, size * nmemb)) div Int64(size);
  except
    Result := 0;
  end;
end;

function ops_seek_func(datasource : pointer; offset: Int64; whence: Integer): Integer; cdecl;
begin
  Result := TVorbisOggDecoder(datasource).DataStream.DoSeek(offset, whence);
end;

function ops_close_func({%H-}datasource : pointer): Integer; cdecl;
begin
  result := 0; 
end;

function ops_tell_func(datasource : pointer): clong; cdecl;
begin
  Result := TVorbisOggDecoder(datasource).DataStream.DoTell;
end;

{ TVorbisOggDecoder }

constructor TVorbisOggDecoder.Create;
begin
  Init;
end;

destructor TVorbisOggDecoder.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TVorbisOggDecoder.Comments : ISoundComment;
begin
  Result := fComm;
end;

function TVorbisOggDecoder.Ready : Boolean;
begin
  Result := Assigned(fRef);
end;

function TVorbisOggDecoder.GetSampleSize : TSoundSampleSize;
begin
  Result := ss16bit;
end;

function TVorbisOggDecoder.GetBitrate : Cardinal;
begin
  Result := VorbisInfo.BitrateNominal;
end;

function TVorbisOggDecoder.GetChannels : Cardinal;
begin
  Result := VorbisInfo.Channels;
end;

function TVorbisOggDecoder.GetFrequency : Cardinal;
begin
  Result := VorbisInfo.Rate;
end;

function TVorbisOggDecoder.GetVersion : Integer;
begin
  Result := VorbisInfo.Version;
end;

{ TVorbisDecoder }

function TVorbisDecoder.Init : Boolean;
begin
  Result := vorbis_synthesis_init(@FRef, Info.Ref) = 0;
end;

constructor TVorbisDecoder.Create(inf : IVorbisInfo);
begin
  inherited Create(inf);
  if not Init then
    raise EVorbis.Create(sCantInitSynthesis);
end;

constructor TVorbisDecoder.CreateNotInit(inf : IVorbisInfo);
begin
  inherited Create(inf);
end;

function TVorbisDecoder.IDHeader(op : IOGGPacket) : Integer;
begin
  Result := vorbis_synthesis_idheader(op.Ref);
end;

function TVorbisDecoder.HeaderIn(vc : ISoundComment; op : IOGGPacket) : Integer;
begin
  Result := vorbis_synthesis_headerin(Info.Ref, vc.Ref, op.Ref);
end;

function TVorbisDecoder.Restart : Integer;
begin
  Result := vorbis_synthesis_restart(Ref);
end;

function TVorbisDecoder.BlockIn(b : IVorbisBlock) : Integer;
begin
  Result := vorbis_synthesis_blockin(Ref, b.Ref);
end;

function TVorbisDecoder.PCMOut(pcm : pointer) : Integer;
begin
  Result := vorbis_synthesis_pcmout(Ref, pcm);
end;

function TVorbisDecoder.LapOut(pcm : pointer) : Integer;
begin
  Result := vorbis_synthesis_lapout(Ref, pcm);
end;

function TVorbisDecoder.Read(samples : Integer) : Integer;
begin
  Result := vorbis_synthesis_read(Ref, samples);
end;

function TVorbisDecoder.BlockSize(op : IOGGPacket) : Longint;
begin
  Result := vorbis_packet_blocksize(Info.Ref, op.Ref);
end;

function TVorbisDecoder.Halfrate(flag : Boolean) : Integer;
begin
  Result := vorbis_synthesis_halfrate(Info.Ref, integer(flag));
end;

function TVorbisDecoder.IsHalfrate : Boolean;
begin
  Result := Boolean(vorbis_synthesis_halfrate_p(Info.Ref));
end;

{ TUniqVorbisComment }

destructor TUniqVorbisComment.Destroy;
begin
  Done;
  inherited Destroy;
end;

{ TVorbisFile }

function TVorbisFile.InitEncoder(aProps : ISoundEncoderProps;
                                 aComments : ISoundComment) : ISoundEncoder;
begin
  Result := TVorbis.NewOggStreamEncoder(Stream, DataLimits, aProps,
                                     aComments as ISoundComment) as ISoundEncoder;
end;

function TVorbisFile.InitDecoder : ISoundDecoder;
begin
  Result := TVorbis.NewOggStreamFileDecoder(Stream, DataLimits) as ISoundDecoder;
end;

{ TVorbisFileAlt }

function TVorbisFileAlt.InitDecoder : ISoundDecoder;
begin
  Result := TVorbis.NewOggStreamAltDecoder(Stream) as ISoundDecoder;
end;

{ TVorbisOggFileDecoder }

procedure TVorbisOggFileDecoder.Init;
var cError : Integer;
begin
  fRef := GetMem(Sizeof(OggVorbisFileDec));
  FillByte(fRef^, Sizeof(OggVorbisFileDec), 0);

  Ref^.callbacks.read_func := @ops_read_func;
  Ref^.callbacks.seek_func := @ops_seek_func;
  Ref^.callbacks.close_func := @ops_close_func;
  Ref^.callbacks.tell_func := @ops_tell_func;

  cError := ov_open_callbacks(Pointer(Self), OVFile, nil, 0, Ref^.callbacks);
  if cError <> 0 then
    raise EVorbis.CreateFmt(sVorbisError, [cError]);

  fComm := TVorbis.RefComment(ov_comment(OVFile, -1));

  Ref^.info := TVorbis.RefInfo(ov_info(OVFile, -1));
end;

procedure TVorbisOggFileDecoder.Done;
begin
  fComm := nil;
  if Assigned(fRef) then
  begin
    Ref^.info := nil;
    if (OVFile^.ready_state <> NOTOPEN) then
      ov_clear(OVFile);
    FreeMemAndNil(fRef);
  end;
end;

function TVorbisOggFileDecoder.OVFile : pOggVorbis_File;
begin
  Result := @(Ref^.ovfile);
end;

function TVorbisOggFileDecoder.VorbisInfo : IVorbisInfo;
begin
  Result := Ref^.info;
end;

function TVorbisOggFileDecoder.Ref : pOggVorbisFileDec;
begin
  Result := pOggVorbisFileDec(fRef);
end;

function TVorbisOggFileDecoder.ReadData(Buffer : Pointer; Sz : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
var
  Section, Res: Integer;
begin
  Section := 0;
  Result := TOGLSound.NewEmptyFrame(Sz);
  while Result.Less(Sz) do begin
    Res := ov_read(fRef, @(PByte(Buffer)[Result.AsBytes]),
                         Sz.AsBytes - Result.AsBytes,
                         0, 2, 1, @Section);
    if Res > 0 then
      Result.IncBytes(Res) else
      break;
  end;
end;

procedure TVorbisOggFileDecoder.ResetToStart;
begin
  ov_pcm_seek(fRef, 0);
end;

procedure TVorbisOggFileDecoder.RawSeek(pos : Int64);
begin
  if DataStream.Seekable then
    ov_raw_seek(fRef, pos) else
    inherited RawSeek(pos);
end;

procedure TVorbisOggFileDecoder.SampleSeek(pos : Integer);
begin
  if DataStream.Seekable then
    ov_pcm_seek(fRef, pos) else
    inherited SampleSeek(pos);
end;

procedure TVorbisOggFileDecoder.TimeSeek(pos : Double);
begin
  if DataStream.Seekable then
    ov_time_seek(fRef, pos) else
    inherited TimeSeek(pos);
end;

function TVorbisOggFileDecoder.RawTell : Int64;
begin
  if DataStream.Seekable then
    Result := ov_raw_tell(fRef)
  else
    Result := inherited RawTell;
end;

function TVorbisOggFileDecoder.SampleTell : Integer;
begin
  if DataStream.Seekable then
    Result := ov_pcm_tell(fRef)
  else
    Result := inherited SampleTell;
end;

function TVorbisOggFileDecoder.TimeTell : Double;
begin
  if DataStream.Seekable then
  begin
    Result := Double(ov_time_tell(fRef));
  end
  else
    Result := inherited TimeTell;
end;

function TVorbisOggFileDecoder.RawTotal : Int64;
begin
  if DataStream.Seekable then
  begin
    Result := ov_raw_total(fRef, -1);
  end
  else
    Result := inherited RawTotal;
end;

function TVorbisOggFileDecoder.SampleTotal : Integer;
begin
  if DataStream.Seekable then
  begin
    Result := ov_pcm_total(fRef, -1);
  end
  else
    Result := inherited SampleTotal;
end;

function TVorbisOggFileDecoder.TimeTotal : Double;
begin
  if DataStream.Seekable then
  begin
    Result := ov_time_total(fRef, -1);
  end
  else
    Result := inherited TimeTotal;
end;

{ TVorbisOggAltDecoder }

function TVorbisOggAltDecoder.ReadDataToOgg : Integer;
var aBuffer : Pointer;
begin
  aBuffer := OggState.Buffer(DecoderReadBufferSize);
  if Assigned(aBuffer) then
  begin
    Result := DataStream.DoRead(aBuffer, DecoderReadBufferSize);
    OggState.Wrote(Result);
  end else
    Result := -1;
end;

procedure TVorbisOggAltDecoder.ReadHeaders;
var i, len, res : Integer;
begin
  len := ReadDataToOgg;
  if len > 0 then
  begin
    Ref^.page := TOGG.NewPage;
    if OggState.PageOut(OggPage) <> 1 then
    begin
      if len >= DecoderReadBufferSize then
        raise EVorbis.Create(sWrongOggStream);
    end;
    Ref^.ostr := TOGG.NewStream(OggPage.SerialNo);
    OggStream.PageIn(OggPage);
    Ref^.pack := TOGG.NewPacket;
    if not OggStream.PacketOut(OggPacket) then
      raise EVorbis.Create(sWrongHeader);

    fcomm := TVorbis.NewComment;
    if Decoder.HeaderIn(fcomm, OggPacket) < 0 then
      raise EVorbis.Create(sNotVorbis);

    i := 0;
    while i < 2 do
    begin
      while i < 2 do
      begin
        res := OggState.PageOut(OggPage);
        if (res = 0) then break; // Need more data
        { Don't complain about missing or corrupt data yet. We'll
          catch it at the packet output phase }
        if (res = 1) then begin
          OggStream.PageInIgnoreErrors(OggPage); { we can ignore any errors here
                                                   as they'll also become
                                                   apparent at packetout }
          while i < 2 do
          begin
            res := OggStream.PacketOutIgnoreErrors(OggPacket);
            if res = 0 then break;
            if res < 0 then
              raise EVorbis.Create(sCorruptSecondaryHeader);
            res := Decoder.HeaderIn(fcomm, OggPacket);
            if res < 0 then
              raise EVorbis.Create(sCorruptSecondaryHeader);
            Inc(i);
          end;
        end;
      end;
      { no harm in not checking before adding more }
      len := ReadDataToOgg;
      if (len = 0) and (i < 2) then
        raise EVorbis.Create(sUnexpectedEndOfFile);
    end;

    if not Decoder.Init then
      raise EVorbis.Create(sCantInitSynthesis);
    Ref^.block := Decoder.InitBlock;
  end else
    raise EVorbis.Create(sWrongOggStream);
end;

procedure TVorbisOggAltDecoder.Init;
begin
  FRef := GetMem(Sizeof(OggVorbisDec));
  FillByte(FRef^, Sizeof(OggVorbisDec), 0);

  Ref^.osyn := TOGG.NewSyncState;
  Ref^.dec  := TVorbisDecoder.CreateNotInit(TVorbis.NewInfo) as IVorbisDecoder;

  Ref^.state := 0;

  OggState.Init;
  ReadHeaders;
end;

procedure TVorbisOggAltDecoder.Done;
begin
  if Assigned(FRef) then
  begin
    Ref^.dec := nil;
    Ref^.block := nil;
    Ref^.osyn := nil;
    Ref^.ostr := nil;
    Ref^.page := nil;
    Ref^.pack := nil;
    FreeMemAndNil(FRef);
  end;
end;

function TVorbisOggAltDecoder.VorbisInfo : IVorbisInfo;
begin
  Result := Decoder.Info;
end;

function TVorbisOggAltDecoder.OggState : IOGGSyncState;
begin
  Result := Ref^.osyn;
end;

function TVorbisOggAltDecoder.OggStream : IOGGStreamState;
begin
  Result := Ref^.ostr;
end;

function TVorbisOggAltDecoder.OggPage : IOGGPage;
begin
  Result := Ref^.page;
end;

function TVorbisOggAltDecoder.OggPacket : IOGGPacket;
begin
  Result := Ref^.pack;
end;

function TVorbisOggAltDecoder.Decoder : IVorbisDecoder;
begin
  Result := Ref^.dec;
end;

class function TVorbisOggAltDecoder.DecoderReadBufferSize : Integer;
begin
  Result := 4096;
end;

function TVorbisOggAltDecoder.Ref : pOggVorbisDec;
begin
  Result := pOggVorbisDec(fRef);
end;

function TVorbisOggAltDecoder.ReadData(Buffer : Pointer; Sz : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
const OGG_DECODE_STATE_EOF = Byte(10);
      OGG_DECODE_STATE_NEXT_PAGE = Byte(0);
      OGG_DECODE_STATE_NEXT_PACKET = Byte(1);
      OGG_DECODE_STATE_NEXT_AUDIO_CHUNK = Byte(2);
var
  res, samples, bout, i, j : Integer;
  ptr : PInt16;
  ch  : pcfloat;
begin
  Result := TOGLSound.NewEmptyFrame(Sz);
  while (Result.Less(Sz)) and (Ref^.state <> OGG_DECODE_STATE_EOF) do
  begin
    case Ref^.state of
      OGG_DECODE_STATE_NEXT_PAGE : begin
         res := OggState.PageOut(OggPage);
         if res = 0 then begin
           if ReadDataToOgg <= 0 then
           begin
             Ref^.state := OGG_DECODE_STATE_EOF;
             break;
           end;
           Ref^.state := OGG_DECODE_STATE_NEXT_PAGE;
         end
         else
         if res < 0 then
           Ref^.state := OGG_DECODE_STATE_NEXT_PAGE
         else
         begin
           OggStream.PageInIgnoreErrors(OggPage);
           Ref^.state := OGG_DECODE_STATE_NEXT_PACKET;
         end;
      end;
      OGG_DECODE_STATE_NEXT_PACKET : begin
          res := OggStream.PacketOutIgnoreErrors(OggPacket);
          if res = 0 then
          begin
            Ref^.state := OGG_DECODE_STATE_NEXT_PAGE;
            if OggPage.EoS then
              Ref^.state := OGG_DECODE_STATE_EOF;
          end
          else
          if res < 0 then
            Ref^.state := OGG_DECODE_STATE_NEXT_PACKET
          else
          begin
            if Ref^.block.Synthesis(OggPacket) = 0 then
               Decoder.BlockIn(Ref^.block);
            Ref^.state := OGG_DECODE_STATE_NEXT_AUDIO_CHUNK;
          end;
      end;
      OGG_DECODE_STATE_NEXT_AUDIO_CHUNK : begin
          samples := Decoder.PCMOut(@(Ref^.pcm_buffer));
          if samples > 0 then
          begin
            res := Sz.AsSamples - Result.AsSamples;

            if res = 0 then Break;

            if samples < res then
              bout := samples else
              bout := res;

            { convert floats to 16 bit signed ints (host order) and
              interleave }
            for i := 0 to GetChannels-1 do
            begin
              ptr := @(PInt16(@(PByte(Buffer)[Result.AsBytes]))[i]);
              ch  := pcfloat(PPointer(Ref^.pcm_buffer)[i]);
              for j := 0 to bout-1 do
              begin
                res := Trunc(ch[j] * 32767.0 + 0.5);

                { might as well guard against clipping }
                if res > 32767 then
                  res := 32767;
                if res < -32768 then
                  res := -32768;

                ptr^ := res;
                Inc(ptr, GetChannels);
              end;
            end;

            Result.IncSamples(bout);

            Decoder.Read(bout);
          end else
            Ref^.state := OGG_DECODE_STATE_NEXT_PACKET;
      end;
    else
      Break;
    end;
  end;
end;

procedure TVorbisOggAltDecoder.ResetToStart;
begin
  inherited ResetToStart;
end;

{ TVorbisOggStreamAltDecoder }

constructor TVorbisOggStreamAltDecoder.Create(aStream : TStream);
begin
  InitStream(TOGLSound.NewDataStream(aStream, [sdpForceNotSeekable]));
  inherited Create;
end;

procedure TVorbisOggStreamAltDecoder.SetStream(aStream : TStream);
begin
  (DataStream as TSoundDataStream).Stream := aStream;
  Ref^.state := 0;
end;

{ TVorbisOggStreamFileDecoder }

constructor TVorbisOggStreamFileDecoder.Create(aStream : TStream;
  aDataLimits : TSoundDataLimits);
begin
  InitStream(TOGLSound.NewDataStream(aStream, aDataLimits));
  inherited Create;
end;

procedure TVorbisOggStreamFileDecoder.SetStream(aStream : TStream);
begin
  (DataStream as TSoundDataStream).Stream := aStream;
end;

{ TVorbisOggStreamEncoder }

procedure TVorbisOggStreamEncoder.WriteOggStream(aFlush : Boolean);
begin
  if aFlush then
    OggStream.PagesFlushToStream((DataStream as TSoundDataStream).Stream) else
    OggStream.PagesOutToStream((DataStream as TSoundDataStream).Stream);
end;

constructor TVorbisOggStreamEncoder.Create(aStream : TStream;
  aDataLimits : TSoundDataLimits;
  aProps : ISoundEncoderProps;
  aComments : ISoundComment);
begin
  InitStream(TOGLSound.NewDataStream(aStream, aDataLimits));
  inherited Create(aProps, aComments);
end;

procedure TVorbisOggStreamEncoder.SetStream(aStream : TStream);
begin
  (DataStream as TSoundDataStream).Stream := aStream;
end;

procedure TVorbisOggStreamEncoder.ReInitEncoder;
begin
  // reset encoder
  OggStream.Reset;
  Encoder.Init;
  fRef^.block := Encoder.InitBlock;
end;

{ TVorbisOggEncoder }

procedure TVorbisOggEncoder.Init(aProps : ISoundEncoderProps;
  aComments : ISoundComment);
var
  aMode : TSoundEncoderMode;
  aSampleSize : TSoundSampleSize;

begin
  aMode := aProps.GetDefault(TOGLSound.PROP_MODE, oemVBR);
  aSampleSize := aProps.GetDefault(TOGLSound.PROP_SAMPLE_SIZE, ss16bit);


  fRef := GetMem(Sizeof(OggVorbisEnc));
  FillByte(fRef^, Sizeof(OggVorbisEnc), 0);

  fRef^.oggs := TOGG.NewStream(Abs(Random(Int64(Now))));
  if Assigned(aComments) then
  begin
    fComm := aComments as ISoundComment;
  end else
    FComm := TVorbis.NewComment;
  fRef^.info := TVorbis.NewInfo;

  if aMode = oemVBR then
  begin
    fRef^.enc := TVorbis.NewEncoderVBR(fRef^.info, aProps.Channels,
                                            aProps.Frequency,
                                            aProps.Quality);
  end else
  if aMode = oemCBR then
  begin
    fRef^.enc := TVorbis.NewEncoderABR(fRef^.info, aProps.Channels,
                                            aProps.Frequency,
                                            -1, aProps.Bitrate, -1);
  end;
  fRef^.block := Encoder.InitBlock;
  fRef^.ssize := aSampleSize;
  fRef^.mode := aMode;
end;

procedure TVorbisOggEncoder.Done;
begin
  if Assigned(fRef) then
  begin
    fRef^.block := nil;
    fRef^.enc := nil;
    fRef^.info := nil;
    fRef^.oggs := nil;
    FreeMemAndNil(fRef);
  end;
  fComm := nil;
end;

function TVorbisOggEncoder.GetSampleSize : TSoundSampleSize;
begin
  Result := fRef^.ssize;
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

function TVorbisOggEncoder.GetMode : TSoundEncoderMode;
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

procedure TVorbisOggEncoder.WriteOggStream({%H-}aFlush : Boolean);
begin
  // do nothing
end;

function TVorbisOggEncoder.Ref : pOggVorbisEnc;
begin
  Result := fRef
end;

constructor TVorbisOggEncoder.Create(aProps : ISoundEncoderProps;
  aComments : ISoundComment);
begin
  Init(aProps, aComments);
end;

destructor TVorbisOggEncoder.Destroy;
begin
  Done;
  inherited Destroy;
end;

function TVorbisOggEncoder.Comments : ISoundComment;
begin
  Result := fComm;
end;

function TVorbisOggEncoder.WriteData(Buffer : Pointer; Count : ISoundFrameSize;
  Par : Pointer) : ISoundFrameSize;
var
  BufW : Pointer;
begin
  if Count.IsValid then
  begin
    BufW := Encoder.GetBuffer(Count.AsSamples);
    UninterleaveSamples(Buffer, PPointer(BufW), GetSampleSize, ssFloat, false,
                                                  GetChannels, Count.AsSamples);
    Encoder.Wrote(Count.AsSamples);

    WriteStreamPages(False);

    Result := TOGLSound.NewFrame(Count);
  end else
    Result := TOGLSound.NewErrorFrame;
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

  WriteOggStream(True);
end;

procedure TVorbisOggEncoder.Close(Par : Pointer);
begin
  Encoder.Wrote(0);

  Flush(Par);
end;

procedure TVorbisOggEncoder.Flush(Par : Pointer);
begin
  WriteStreamPages(True);
  WriteOggStream(True);
end;

procedure TVorbisOggEncoder.WriteStreamPages(aFlush : Boolean);
var
  og : IOGGPacket;
begin
  og := TOgg.NewPacket;

  while (Encoder.BlockOut(fRef^.block) = 1) do
  begin
    fRef^.block.Analysis(nil);
    fRef^.block.BitrateAdd;

    while (Encoder.BitrateFlushPacket(og) <> 0) do
    begin
      OggStream.PacketIn(og);
      WriteOggStream(aFlush);
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

function TVorbisState.InitBlock : IVorbisBlock;
begin
  Result := TVorbisBlock.Create as IVorbisBlock;
  vorbis_block_init(Ref, Result.Ref);
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

procedure TVorbisEncoder.HeaderOut(vc : ISoundComment; header, header_comm,
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

function TVorbisBlock.Synthesis(OgPac : IOGGPacket) : integer;
begin
  if Assigned(OgPac) then
    Result := vorbis_synthesis(@FRef, OgPac.Ref) else
    Result := vorbis_synthesis(@FRef, nil);
end;

function TVorbisBlock.TrackOnly(OgPac : IOGGPacket) : Integer;
begin
  Result := vorbis_synthesis_trackonly(@FRef, OgPac.Ref);
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

procedure TRefVorbisComment.SetVendor(const {%H-}S : String);
begin
  // not supported for libvorbisenc
end;

procedure TRefVorbisComment.SetNativeVendor({%H-}v : PChar);
begin
  // not supported for libvorbisenc
end;

function TRefVorbisComment.GetNativeVendor : PChar;
begin
  Result := PChar(fRef^.vendor);
end;

function TRefVorbisComment.GetNativeComment(index : integer) : PChar;
begin
  Result := PChar(fRef^.user_comments[index]);
end;

function TRefVorbisComment.GetNativeCommentLength(index : integer) : Int32;
begin
  Result := fRef^.comment_lengths[index];
end;

function TRefVorbisComment.GetNativeCommentCount : Int32;
begin
  Result := fRef^.comments;
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
  inherited Add(comment);
end;

procedure TRefVorbisComment.AddTag(const tag, value : String);
begin
  vorbis_comment_add_tag(FRef, pcchar(pchar(tag)), pcchar(pchar(value)));
  inherited AddTag(tag, value);
end;

function TRefVorbisComment.Query(const tag : String; index : Integer) : String;
begin
  Result := StrPas( PChar(vorbis_comment_query(FRef, pcchar(PChar(tag)), index)) );
end;

function TRefVorbisComment.QueryCount(const tag : String) : Integer;
begin
  Result := vorbis_comment_query_count(FRef, pcchar(PChar(tag)));
end;

{ TVorbis }

class function TVorbis.NewComment : ISoundComment;
begin
  Result := TUniqVorbisComment.Create as ISoundComment;
end;

class function TVorbis.NewComment(src : ISoundComment) : ISoundComment;
begin
  Result := TUniqVorbisComment.CreateFromInterface(src) as ISoundComment;
end;

class function TVorbis.RefComment(aRef : pvorbis_comment) : ISoundComment;
begin
  Result := TRefVorbisComment.Create(aRef) as ISoundComment;
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

class function TVorbis.NewDecoder(inf : IVorbisInfo) : IVorbisDecoder;
begin
  Result := TVorbisDecoder.Create(inf) as IVorbisDecoder;
end;

class function TVorbis.NewOggStreamEncoder(aStream : TStream;
  aDataLimits : TSoundDataLimits;
  aProps : ISoundEncoderProps; aComments : ISoundComment) : ISoundStreamEncoder;
begin
  Result := TVorbisOggStreamEncoder.Create(aStream, aDataLimits, aProps, aComments);
end;

class function TVorbis.NewOggStreamFileDecoder(aStream : TStream;
    aDataLimits : TSoundDataLimits) : ISoundStreamDecoder;
begin
  Result := TVorbisOggStreamFileDecoder.Create(aStream, aDataLimits);
end;

class function TVorbis.NewOggStreamAltDecoder(aStream : TStream) : ISoundStreamDecoder;
begin
  Result := TVorbisOggStreamAltDecoder.Create(aStream);
end;

class function TVorbis.EncoderVersionString: String;
begin
  Result := StrPas(PChar(vorbis_version_string()));
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

