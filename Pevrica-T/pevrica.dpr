program pevrica;

uses
  Forms,
  pevfrm in 'pevfrm.pas' {Form1},
  pevcoder in 'pevcoder.pas',
  bfs in 'bfs.pas',
  mwHashStrings in 'mwHashStrings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
