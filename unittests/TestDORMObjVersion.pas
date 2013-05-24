unit TestDORMObjVersion;

interface

uses
  testframework, dorm, dorm.Commons, BaseTestCase;

type
  TTestObjVersion = class(TBaseTestCase)
  published
    procedure Test_Create_Versioned_Object;
    procedure Test_Update_Versioned_Object;
    procedure Test_Delete_Versioned_Object;
    procedure Test_OffLine_Change;
    procedure Test_OffLine_Update_Deleted_Object;
    procedure Test_OffLine_Delete_Deleted_Object;
  end;

implementation

{ TTestObjVersion }

uses dorm.tests.bo, System.SysUtils, dorm.ObjectStatus;

procedure TTestObjVersion.Test_Create_Versioned_Object;
var
  todo1: TToDo;
begin
  todo1 := TToDo.Create;
  try
    todo1.Description := 'buy new phone';
    todo1.DueDate := encodedate(2013, 1, 1);
    todo1.DueTime := encodetime(12, 0, 0, 0);
    todo1.Done := false;
    Session.Persist(todo1);
    CheckEquals(1, todo1.ObjVersion);
  finally
    todo1.Free;
  end;
end;

procedure TTestObjVersion.Test_Delete_Versioned_Object;
var
  todo1: TToDo;
  id: Integer;
  todo2: TToDo;
begin
  todo1 := TToDo.Create;
  try
    todo1.Description := 'buy new phone';
    todo1.DueDate := encodedate(2013, 1, 1);
    todo1.DueTime := encodetime(12, 0, 0, 0);
    todo1.Done := false;
    Session.Persist(todo1);
    id := todo1.id;
  finally
    todo1.Free;
  end;

  todo2 := Session.Load<TToDo>(id);
  try
    CheckEquals(1, todo2.ObjVersion);
    todo2.ObjStatus := TdormObjectStatus.osDeleted;
    Session.Persist(todo2);
    CheckTrue(todo2.ObjStatus = TdormObjectStatus.osDirty);
    CheckFalse(Session.OIDIsSet(todo2));
  finally
    todo2.Free;
  end;
end;

procedure TTestObjVersion.Test_OffLine_Change;
var
  s1: TSession;
  s2: TSession;
  todo1: TToDo;
  todo2: TToDo;
begin
  s1 := CreateSession;
  try
    todo1 := TToDo.Create;
    try
      todo1.Description := 'buy new phone';
      todo1.DueDate := encodedate(2013, 1, 1);
      todo1.DueTime := encodetime(12, 0, 0, 0);
      todo1.Done := false;
      Session.Persist(todo1);
      Session.Commit(true);

      // this is the 2nd session that will change the object and save it before the 1st
      s2 := CreateSession;
      try
        todo2 := s2.Load<TToDo>(todo1.id);
        try
          todo2.Description := 'changed from 2nd session';
          todo2.ObjStatus := osDirty;
          s2.Persist(todo2);
        finally
          todo2.Free;
        end;
      finally
        s2.Free;
      end;

      // now, saving the 1st object from the 1st session should raise a locking error
      todo1.Description := 'changed from 1st session';
      todo1.ObjStatus := osDirty;
      ExpectedException := EdormLockingException;
      s1.Persist(todo1);
    finally
      todo1.Free;
    end;
  finally
    s1.Free;
  end;
end;

procedure TTestObjVersion.Test_OffLine_Update_Deleted_Object;
var
  s1: TSession;
  s2: TSession;
  todo1: TToDo;
  todo2: TToDo;
begin
  s1 := CreateSession;
  try
    todo1 := TToDo.Create;
    try
      todo1.Description := 'buy new phone';
      todo1.DueDate := encodedate(2013, 1, 1);
      todo1.DueTime := encodetime(12, 0, 0, 0);
      todo1.Done := false;
      Session.Persist(todo1);
      Session.Commit(true);

      // this is the 2nd session that will delet the object and save it before the 1st
      s2 := CreateSession;
      try
        todo2 := s2.Load<TToDo>(todo1.id);
        try
          todo2.ObjStatus := osDeleted;
          s2.Persist(todo2);
        finally
          todo2.Free;
        end;
      finally
        s2.Free;
      end;

      // now, saving the 1st object from the 1st session should raise a locking error
      todo1.Description := 'changed from 1st session';
      todo1.ObjStatus := osDirty;
      ExpectedException := EdormLockingException;
      s1.Persist(todo1);
    finally
      todo1.Free;
    end;
  finally
    s1.Free;
  end;
end;

procedure TTestObjVersion.Test_OffLine_Delete_Deleted_Object;
var
  s1: TSession;
  s2: TSession;
  todo1: TToDo;
  todo2: TToDo;
begin
  s1 := CreateSession;
  try
    todo1 := TToDo.Create;
    try
      todo1.Description := 'buy new phone';
      todo1.DueDate := encodedate(2013, 1, 1);
      todo1.DueTime := encodetime(12, 0, 0, 0);
      todo1.Done := false;
      Session.Persist(todo1);
      Session.Commit(true);

      // this is the 2nd session that will delet the object and save it before the 1st
      s2 := CreateSession;
      try
        todo2 := s2.Load<TToDo>(todo1.id);
        try
          todo2.ObjStatus := osDeleted;
          s2.Persist(todo2);
        finally
          todo2.Free;
        end;
      finally
        s2.Free;
      end;

      // now, saving the 1st object from the 1st session should raise a locking error
      todo1.ObjStatus := osDeleted;
      ExpectedException := EdormLockingException;
      s1.Persist(todo1);
    finally
      todo1.Free;
    end;
  finally
    s1.Free;
  end;
end;

procedure TTestObjVersion.Test_Update_Versioned_Object;
var
  todo1: TToDo;
  id: Integer;
  todo2: TToDo;
begin
  todo1 := TToDo.Create;
  try
    todo1.Description := 'buy new phone';
    todo1.DueDate := encodedate(2013, 1, 1);
    todo1.DueTime := encodetime(12, 0, 0, 0);
    todo1.Done := false;
    Session.Persist(todo1);
    id := todo1.id;
  finally
    todo1.Free;
  end;

  todo2 := Session.Load<TToDo>(id);
  try
    CheckEquals(1, todo2.ObjVersion);
    todo2.ObjStatus := TdormObjectStatus.osDirty;
    Session.Persist(todo2);
    CheckEquals(2, todo2.ObjVersion);
  finally
    todo2.Free;
  end;

end;

initialization

RegisterTest(TTestObjVersion.Suite);

end.
