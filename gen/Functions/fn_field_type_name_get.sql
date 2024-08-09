/*
Возвращает строку определения типа

select [gen].[fn_field_type_name_get]('numeric', 108, 108, 17, 38, 38)

*/
create   function [gen].[fn_field_type_name_get]
(
  @type_name      sysname,
  @system_type_id tinyint,
  @user_type_id   int,
  @max_length     int,
  @precision      tinyint,
  @scale          tinyint
)
returns sysname
as
begin
  return
  (
    case
      when charindex(' ', @type_name) > 0 or quotename(@type_name) <> '[' + @type_name + ']'
        then quotename(@type_name)
        else @type_name
    end +
    case
      when @system_type_id = @user_type_id
        and @type_name in ('char', 'varchar', 'binary', 'varbinary')
        then concat(N'(', iif(@max_length < 0, N'max', cast(@max_length as nvarchar)), N')')
      when @system_type_id = @user_type_id
        and @type_name in ('nchar', 'nvarchar')
        then concat(N'(', iif(@max_length < 0, N'max', cast(@max_length/2 as nvarchar)), N')')
      when @system_type_id = @user_type_id
        and @type_name = 'float'
        then concat(N'(', @precision, N')')
      when @system_type_id = @user_type_id
        and @type_name in ('decimal', 'numeric')
        then concat(N'(', @precision, N',', @scale, N')')
        else N''
    end
  )
end
