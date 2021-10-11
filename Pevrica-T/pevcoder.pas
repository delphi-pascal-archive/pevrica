unit pevcoder;

(*************************************************************************

PEVRICA-T TEXT COMPRESSION ALGORITHM

Algorithm is educational, non-standart, fully made by me.

Low-compression, very simple algorithm with ulta-fast decompression

Uses one internal pre-made dictionary

(c)  2009 Alexander Myasnikow

Russia, Vladimir region, Kolchugino

WEB: www.darksoftware.narod.ru

E-mail: darksoftware@ya.ru

Free for non-commercial usage

If you like this code and want to use it

in commercial software, please contact author.

*************************************************************************)


interface


uses Classes, Windows, SysUtils, Math, inifiles, mwHashStrings;

type
  TPevCoder = class

  type
    TBitA = array of byte;
  var
    BitA:    TBitA;
    BitAPos: integer;

  const
    header: word = $FEAD;


    procedure LoadToBA (fn: string);
    procedure SaveToBA (fn: string);
    procedure SavePacked (fn: string);
    procedure LoadPacked (fn: string);
    function GetBit: byte;
    procedure PutBit (b: byte);
    procedure PutByte (b: byte);
    procedure PutInteger (b: longint);
    procedure PutWord (b: word);
    function GetByte (): byte;
    function GetInteger (): longint;
    function GetWord (): word;
    function GetFileSize (FileName: string): integer;
    function Findmaxlen (s: string): integer;
    procedure LoadDictionary (fn: string);


    constructor Create;
    destructor Destroy; override;

  var
    index: TmwHashedStringList;

  end;

implementation

uses bfs, pevfrm;

procedure TPevCoder.LoadDictionary (fn: string);
  begin
    index.Capacity := 262143;
    index.LoadFromFile(fn);
  end;

constructor TPevCoder.Create;
  begin
    BitAPos := 0;
    index := TmwHashedStringList.Create;
    index.CaseSensitive := True;
  end;


destructor TPevCoder.Destroy;
  begin
    SetLength(BitA, 0);
    index.Free;
    inherited Destroy;
  end;


function TPevCoder.GetBit: byte;
  begin
    if BitAPos < Length(BitA) then
      begin
      Result  := BitA[BitAPos];
      BitAPos := BitAPos + 1;
      end
    else
      begin
      Result := random(256) and 1;
      end;
  end;

procedure TPevCoder.PutBit (b: byte);
  begin
    if (Length(BitA) - BitAPos) < 32768 then
      SetLength(BitA, Length(BitA) + 32768);

    BitA[BitAPos] := b;
    BitAPos := BitAPos + 1;

  end;


procedure TPevCoder.PutByte (b: byte);
  var
    n: integer;
  begin

    for n := 0 to 7 do
      begin
      BitA[BitAPos] := (b shr n) and 1;
      Inc(BitAPos);
      end;
  end;


procedure TPevCoder.PutInteger (b: longint);
  var
    n: integer;
  begin

    for n := 0 to 31 do
      begin
      BitA[BitAPos] := (b shr n) and 1;
      Inc(BitAPos);
      end;
  end;


procedure TPevCoder.PutWord (b: word);
  var
    n: integer;
  begin

    for n := 0 to 15 do
      begin
      BitA[BitAPos] := (b shr n) and 1;
      Inc(BitAPos);
      end;
  end;


function TPevCoder.GetByte (): byte;
  var
    n: integer;
  begin

    Result := 0;

    BitAPos := BitAPos + 8;

    for n := 1 to 8 do
      begin
      Result := Result shl 1 + BitA[BitAPos - n];
      end;

  end;

function TPevCoder.GetInteger (): longint;
  var
    n: integer;
  begin

    Result := 0;

    BitAPos := BitAPos + 32;

    for n := 1 to 32 do
      begin
      Result := Result shl 1 + BitA[BitAPos - n];
      end;

  end;


function TPevCoder.GetWord (): word;
  var
    n: integer;
  begin

    Result := 0;

    BitAPos := BitAPos + 16;

    for n := 1 to 16 do
      begin
      Result := Result shl 1 + BitA[BitAPos - n];
      end;

  end;


function TPevCoder.findmaxlen (s: string): integer;
  begin
    Result := -1;

    repeat
      Result := index.IndexOf(s);
      if Result <> -1 then
        break;
      Delete(s, length(s), 1);
    until s = '';

  end;

procedure TPevCoder.LoadToBA (fn: string);
  var
    FS: TBFileStream;
  var
    i, n: integer;
    d:  byte;
    f:  integer;
    notfound, ready: boolean;
    block: array [0..14] of char;
    len, ml, fp: integer;

  begin

    FS := TBFileStream.Create(fn, fmOpenRead);
    SetLength(BitA, fs.Size * 8 + 48 + 16 + fs.Size);
    BitAPos := 0;
    PutWord(Header);
    PutInteger(fs.Size);

    ready := False;

    fp := 0;

    repeat


      fillchar(block, 15, 0);
      FS.Seek(fp, 0);
      len := FS.Read(block, 14);
      ml  := findmaxlen(block);
      if ml = -1 then
        begin
        PutBit(0);
        PutByte(byte(block[0]));
        Inc(fp, 1);
        end
      else
        begin
        PutBit(1);
        for n := 0 to 17 do
          PutBit(ml shr n and 1);

        Inc(fp, length(index[ml]));

        end;


    until len = 0;


    FS.Free;
  end;


procedure TPevCoder.SaveToBA (fn: string);
  var
    FS: TBFileStream;
    i, size, done, n: integer;
    d:  byte;
    bits: array [0..19] of byte;
    block: array [0..14] of byte;
    ml: integer;

  begin
    FS := TBFileStream.Create(fn, fmCreate);
    BitAPos := 16;
    size := GetInteger;


    done := 0;
    while True do
      begin

      case GetBit() of
        0:
          begin
          d := GetByte();
          FS.Write(d, 1);
          Inc(done, 1);

          end;
        1:
          begin

          ml := 0;
          for n := 17 downto 0 do
            begin
            bits[n] := GetBit();
            end;

          for n := 0 to 17 do
            begin
            ml := ml shl 1 + Bits[n];
            end;

          fillchar(block, 15, 0);
          StrPCopy(@block[0], index[ml]);
          Inc(done, length(index[ml]));
          FS.Write(block[0], length(index[ml]));

          end;

        end;


      if done >= size then
        break;
      end;

    FS.SetSize(Size);
    FS.Free;
  end;

procedure TPevCoder.SavePacked (fn: string);
  var
    FS: TBFileStream;
    size: integer;
    b:  byte;
    i:  integer;
  begin
    size := Ceil(bitapos / 8);
    BitAPos := 0;
    FS := TBFileStream.Create(fn, fmCreate);
    for I := 0 to size - 1 do
      begin
      b := getbyte;
      FS.Write(b, 1);
      end;
    FS.Free;
  end;

function TPevCoder.GetFileSize (FileName: string): integer;
  var
    FS: TBFileStream;
  begin
      try
      FS := TBFileStream.Create(Filename, fmOpenRead);
      except
      Result := -1;
      end;
    if Result <> -1 then
      Result := FS.Size;
    FS.Free;
  end;

procedure TPevCoder.LoadPacked (fn: string);
  var
    FS: TBFileStream;
    fsize, i: integer;
    b:  byte;
  begin
    BitAPos := 0;
    fsize := GetFileSize(fn);
    FS := TBFileStream.Create(fn, fmOpenRead);
    SetLength(BitA, fsize * 8);
    for I := 0 to Fsize - 1 do
      begin
      FS.Read(b, 1);
      PutByte(b);
      end;
    FS.Free;
  end;


end.
