unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI, FireDAC.Phys.IBBase,
  FireDAC.Phys.FB, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Vcl.StdCtrls, Vcl.ExtCtrls,
  FireDAC.Phys.ODBCBase, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.FBDef;

type
  TMainForm = class(TForm)
    FDConnection1: TFDConnection;
    Panel1: TPanel;
    Panel2: TPanel;
    Memo1: TMemo;
    Button2: TButton;
    qry: TFDQuery;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Memo2: TMemo;
    Edit1: TEdit;
    Edit2: TEdit;
    Splitter1: TSplitter;
    Memo3: TMemo;
    Splitter2: TSplitter;
    Label2: TLabel;
    FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    NoPropertyCase: TCheckBox;
    Edit3: TEdit;
    NoId: TCheckBox;
    NoObjVersion: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Buff: TStringStream;
    FHistoryFileName: String;
    procedure EmitProperty(F: TField);
    procedure EmitClass;
    procedure EmitClassEnd;
    function GetDelphiType(FT: TFieldType): String;
    function GetProperCase(const Value: String): String;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  {Spring.SystemUtils,} System.IOUtils;

{$R *.dfm}

procedure TMainForm.Button2Click(Sender: TObject);
var
  i: Integer;
begin
  FDConnection1.Close;
  FDConnection1.Params.Assign(Memo3.Lines);
  FDConnection1.Connected := True;
  qry.Open(Memo1.Lines.Text);
  Buff.Clear;
  EmitClass;
  for i := 0 to qry.Fields.Count - 1 do
  begin
    if (uppercase(qry.Fields[i].FieldName) <> 'ID') and
      (uppercase(qry.Fields[i].FieldName) <> 'OBJVERSION') then
      EmitProperty(qry.Fields[i])
    else if (uppercase(qry.Fields[i].FieldName) = 'ID') and (not NoId.Checked)
    then
      EmitProperty((qry.Fields[i]))
    else if (uppercase(qry.Fields[i].FieldName) = 'OBJVERSION') and
      (not NoObjVersion.Checked) then
      EmitProperty((qry.Fields[i]));
  end;
  EmitClassEnd;
  Memo2.Lines.Text := Buff.DataString;
  qry.Close;

  Memo3.Lines.SaveToFile(FHistoryFileName);
end;

procedure TMainForm.EmitClass;
begin
  Buff.WriteString('[MapperJSONNaming(JSONNameLowerCase)]' + sLineBreak);
  if trim(Edit2.Text) <> '' then
  begin
    Buff.WriteString(Format('[Entity(''%s'')]', [Edit2.Text]) + sLineBreak);
  end;

  if trim(Edit1.Text) = '' then
    raise Exception.Create('Invalid class name');
  if (Edit3.Text <> '') and (LowerCase(Edit3.Text) <> 'inheritedclass') then
    Buff.WriteString(Edit1.Text + ' = class(TBaseV)' + sLineBreak)
  else
    Buff.WriteString(Edit1.Text + ' = class' + sLineBreak);
  Buff.WriteString('public' + sLineBreak);
end;

procedure TMainForm.EmitClassEnd;
begin
  Buff.WriteString('end;' + sLineBreak);
end;

procedure TMainForm.EmitProperty(F: TField);
begin
  if NoPropertyCase.Checked then
    Buff.WriteString(Format('  [Column(''%s'')]', [F.FieldName]) + sLineBreak +
      '  property ' + F.FieldName + ': ' + GetDelphiType(F.DataType) + ';' +
      sLineBreak)
  else
    Buff.WriteString(Format('  [Column(''%s'')]', [F.FieldName]) + sLineBreak +
      '  property ' + GetProperCase(F.FieldName) + ': ' +
      GetDelphiType(F.DataType) + ';' + sLineBreak);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Buff.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Buff := TStringStream.Create;
  FHistoryFileName := TPath.GetDocumentsPath + PathDelim + 'eg.history';
  try
    if TFile.Exists(FHistoryFileName) then
      Memo3.Lines.LoadFromFile(FHistoryFileName)
    else
      Memo3.Lines.Assign(FDConnection1.Params);
  except

  end;
end;

function TMainForm.GetDelphiType(FT: TFieldType): String;
begin
  case FT of
    ftString:
      Result := 'String';
    ftSmallint, ftInteger, ftWord, ftLongWord, ftShortint:
      Result := 'Integer';
    ftByte:
      Result := 'Byte';
    ftLargeint:
      Result := 'Int64';
    ftBoolean:
      Result := 'Boolean';
    ftFloat, ftSingle, ftExtended:
      Result := 'Double';
    ftCurrency, ftBCD:
      Result := 'Currency';
    ftDate:
      Result := 'TDate';
    ftTime:
      Result := 'TTime';
    ftDateTime:
      Result := 'TDateTime';
    ftTimeStamp:
      Result := 'TDateTime {timestamp}';
    ftAutoInc:
      Result := 'Integer; {autoincrement}';
    ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftWideMemo, ftStream:
      Result := 'TStream';
    ftFixedChar:
      Result := 'String; {fixedchar}';
    ftWideString:
      Result := 'String';
  else
    Result := '<UNSUPPORTED TYPE>'; // + TEnum.GetName<TFieldType>(FT) + '>';
  end;
end;

function TMainForm.GetProperCase(const Value: String): String;
var
  Pieces: TArray<String>;
  s: String;
begin
  if Value.Length <= 2 then
    Exit(Value.ToUpper);

  Pieces := Value.ToLower.Split(['_']);
  for s in Pieces do
  begin
    if s = 'id' then
      Result := Result + 'ID'
    else
      Result := Result + uppercase(s.Chars[0]) + s.Substring(1);
  end;
end;

end.
