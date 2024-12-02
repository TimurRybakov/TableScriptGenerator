/*
Процедура форматирования текста, полученного из таблиц результатов Microsoft SQL Server Management Studio / csv / буфера обменя при копировании из таблиц Excel;

declare @text_with_tabs nvarchar(max) = N'LEVEL' + nchar(9) + 'SCNAME' + nchar(9) + 'SOCRNAME' + nchar(9) + 'KOD_T_ST
1' + nchar(9) + 'АО' + nchar(9) + 'Автономный округ' + nchar(9) + '101
1' + nchar(9) + 'Аобл' + nchar(9) + 'Автономная область' + nchar(9) + '102
1' + nchar(9) + 'г' + nchar(9) + 'Город' + nchar(9) + '103
1' + nchar(9) + 'край' + nchar(9) + 'Край' + nchar(9) + '104
1' + nchar(9) + 'обл' + nchar(9) + 'Область' + nchar(9) + '105
1' + nchar(9) + 'округ' + nchar(9) + 'Округ' + nchar(9) + '107
1' + nchar(9) + 'Респ' + nchar(9) + 'Республика' + nchar(9) + '106
1' + nchar(9) + 'Чувашия' + nchar(9) + 'Чувашия' + nchar(9) + '108
2' + nchar(9) + 'АО' + nchar(9) + 'Автономный округ' + nchar(9) + '205
2' + nchar(9) + 'п' + nchar(9) + 'Поселение' + nchar(9) + '206
2' + nchar(9) + 'р-н' + nchar(9) + 'Район' + nchar(9) + '201
2' + nchar(9) + 'тер' + nchar(9) + 'Территория' + nchar(9) + '203'

EXEC [gen].[text_to_table] @text_with_tabs

EXEC [gen].[text_to_table]
'46 174,50	500,00
60 026,85	650,00
73 509,80	796,00'

*/
CREATE procedure [gen].[text_to_table]
  @text          nvarchar(max),    -- текст для форматирования
  @row_separator nchar(1) = null,  -- разделитель столбцов
  @col_separator nchar(1) = null,  -- разделитель колонок
  @quote_strings bit = 1           -- квотировать выражения, принимаемые как строковые
as
  set nocount on
 
  declare @t table
  (
    [col_index]         int           not null,
    [row_index]         int           not null,
    [value]             nvarchar(max) not null,
    [max_len_by_column] int           not null
    primary key clustered ([row_index], [col_index])
  )
 
  declare
    @max_row int

  if @row_separator is null
    set @row_separator = nchar(10)

  if @col_separator is null
    set @col_separator = nchar(9)

  insert @t
  select
    [col_index],
    [row_index],
    [value],
    [max_len_by_column]  = max(len([value])) over (partition by [col_index])
  from
  (
    select
    [col_index]    = [column].[index],
    [row_index]    = [row].[index],
    [value]       = case
                      when min([is_numeric].[value]) over (partition by [column].[index]) = 0 and @quote_strings = 1
                        then [trimmed&quoted].[value]
                        else [trimmed].[value]
                    end
    from
    (
      select
        [index] = row_number() over (order by (select null)),
        [value] = [value]
      from string_split(@text, @row_separator)
    ) [row]
    cross apply
    (
      select
        [index] = row_number() over (order by (select null)),
        [value] = [value]
      from string_split([row].[value], @col_separator)
    ) [column]
    cross apply
    (
      values (trim(nchar(10) + nchar(13) + nchar(32) from [column].[value]))
    ) [trimmed] ([value])
    cross apply
    (
      values (case
                when [trimmed].[value] <> N'null' collate Cyrillic_General_CI_AS
                  then quotename([trimmed].[value], N'''')
                  else [trimmed].[value]
              end)
    ) [trimmed&quoted] ([value])
    cross apply
    (
      values (iif(try_Cast([trimmed].[value] as float) is null or left([trimmed].[value], 1) = '0' and right([trimmed].[value], 1) <> '0', 0, 1))
    ) [is_numeric] ([value])
  ) q

  select
    @max_row = max([row_index])
  from @t
 
  set @text =
  (
    select
      (
        select
          case
            when [col_index] = 1
              then N'('
              else N''
          end + [value] +
          case
            when [col_index] < max([col_index]) over (partition by [row_index])
              then N',' + replicate(N' ', [max_len_by_column] - len([value])) + ' '
              else N')' -- конец строки
                + case
                    when [row_index] < @max_row
                      then N',' + nchar(13)
                      else N'' -- конец последнего столбца последней строки
                  end
          end
        from @t [columns]
        where [row_index] = [rows].[row_index]
        for xml path(''), type
      )
    from @t [rows]
    group by [row_index]
    for xml path(''), type
  ).value(N'.', N'nvarchar(max)')
 
  exec [gen].[print] @text
