type MyRecordType of record { a: integer, b: integer };

func main () => integer
begin

  var my_record = MyRecordType { a = 3, b = 100 };
  my_record.a = 42;
  assert my_record.a == 42 && my_record.b == 100;

  return 0;
end;
