unit bfs;

interface

uses Classes, SysUtils;

const
  buffersize = 65536;

type
  TBFileStream = class (TObject)
    FileName:    string;
    FS:          TFileStream;
    ABuffer:     array [0..buffersize] of byte;
    AReadBuffer: array [0..buffersize] of byte;
    sizedone:    longint;
    lforward:    longint;
    bufferpos:   longint;
    pos:         longint;
    constructor Create (const AFileName: string; Mode: word);
    destructor Destroy (); override;
    procedure Free ();
    function Write (const Buffer; Count: longint): longint;
    function Read (var Buffer; Count: longint): longint;
    function Size: int64;
    procedure Seek (offset: int64; origin: integer);
    procedure SetSize (NewSize: int64);
    procedure Flush;
    procedure FlushRead;
    procedure NextBuffer (var Buffer; Count: longint);
  end;

implementation


constructor TBFileStream.Create;
  begin
    FileName := AFileName;
    Fs  := TFileStream.Create(FileName, mode);
    sizedone := 0;
    lforward := 0;
    bufferpos := 0;
    pos := 0;
  end;

procedure TBFileStream.Free;
  begin
    Destroy();
  end;

destructor TBFileStream.Destroy;
  begin
    Flush;
    Fs.Free;
    inherited Destroy;
  end;


function TBFileStream.Write (const Buffer; Count: longint): longint;
  begin
    if Count >= buffersize then
      begin
      Flush;
      Result := Fs.Write(Buffer, Count);
      end
    else
      begin
      if sizedone + Count > buffersize then
        begin
        Flush;
        sizedone := Count;
        Move(Buffer, ABuffer, Count);
        Result := Count;
        end
      else
        begin
        Move(Buffer, ABuffer[sizedone], Count);
        Inc(sizedone, Count);
        Result := Count;
        end;

      end;

  end;

procedure TBFileStream.NextBuffer (var Buffer; Count: integer);
  begin
    move(AReadBuffer[BufferPos], Buffer, Count);
    Inc(bufferpos, Count);
  end;

function TBFileStream.Read (var Buffer; Count: longint): longint;

  begin

    Result := 0;

    Flush;

    if lforward = 0 then
      begin
      FS.Position := pos;
      Result := FS.Read(Buffer, Count);
      Inc(pos, Count);
      lforward  := FS.Read(AReadBuffer, buffersize);
      bufferpos := 0;
      end
    else

      begin

      if lforward >= Count then
        begin

        NextBuffer(Buffer, Count);
        Inc(pos, Count);
        Dec(lforward, Count);
        Result := Count;

        end
      else
        begin

        NextBuffer(Buffer, lforward);
        Inc(pos, lforward);
        FS.Position := pos;
        FS.Read(TByteArray(Buffer)[lforward], Count - lforward);
        Inc(pos, Count - lforward);
        bufferpos := 0;
        lforward  := FS.Read(AReadBuffer, buffersize);

        end;

      end;

  end;

procedure TBFileStream.SetSize;
  begin
    Flush;
    Fs.Size := NewSize;
  end;

function TBFileStream.Size;
  begin
    Flush;
    Result := Fs.Size;
  end;


procedure TBFileStream.Seek;
  begin
    Flush;
    Fs.Seek(offset, origin);
  end;

procedure TBFileStream.Flush;
  begin
    if sizedone > 0 then
      begin
      Fs.Write(ABuffer, sizedone);
      sizedone := 0;
      end;
  end;

procedure TBFileStream.FlushRead;
  begin
    lforward  := 0;
    bufferpos := 0;
    FS.Position := pos;
  end;


end.
