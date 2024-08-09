/*

Генератор sql-сркипта для таблиц и/или запросов на их наполнение

Создает скрипт объявления табличной переменной и/или наполнения ее данными в соответствии с физической таблицей, предсатвлением, табличной функцией;

EXEC [gen].[table_script] @object_name_or_query = '<OBJECT NAME>'

*/
------------------------------------------------------------------------------------------------------------------------------------------------------------------
create   procedure [gen].[table_script]
  @object_name_or_query           nvarchar(max),    -- Имя объекта (таблицы, представления, табличной функции, табличного типа), либо текст запроса, результаты которого необходимо представить в виде таблицы
--oo = output object----------------------------------------------------------------------------------------------------------------------------------------------
  @oo_type                        char(2)   = 'TV', -- Тип возвращаемой табличной структуры: 'TV' - табличная переменная, 'Me' - только метаданные по столбцам и ключам, 'PT' - физическая таблица, null - не возвращать скрипт таблицы
  @oo_name                        sysname   = null, -- Имя описываемого объекта. Если null, генерится имя как таличная переменная от имени скриптуемого объекта
  @oo_field_name_max_len          int       = 800,  -- Максимальная длина строки названия поля таблицы, до которой дополнять пробелами (null - не ограничена)
  @oo_field_type_name_def_max_len int       = null, -- Максимальная длина строки типа+определения поля таблицы, до которой дополнять пробелами, по умолчанию = select len('varchar(8000) collate ' + (select top (1) [collation_name] from sys.databases with(nolock) where [database_id] = db_id()))
  @oo_max_data_rows               int       = 1000, -- Максимальное число записей с данными, которые использовать для наполнения таблицы
  @oo_ignore_computed_columns     bit       = 1,    -- Игнорировать ли вычисляемые поля при генерации скрипта возвращаемой табличной структуры
  @oo_column_keys_to_table_keys   bit       = 0,    -- Преображать ли ключи колонок в табличные ключи
  @oo_script                      nvarchar(max) = null out, -- Результирующий скрипт табличной структуры
--df = data fill--------------------------------------------------------------------------------------------------------------------------------------------------
  @df_type                        char(2)   = 'M',  -- Тип скрипта заполнения данными - I(nsert), M(erge), IM (Insert и закомментированный Merge). При null таблица не заполняется
  @df_comment_field_value_columns bit       = 1,    -- Комментировать ли названиями полей колонки значений в скрипте заполнения данными
  @df_field_value_column_max_len  int       = null, -- Максимальная длина строки значения для колонки (null - не ограничена) в скрипте заполнения данными
  @df_script                      nvarchar(max) = null out, -- Результирующий скрипт заполнения данными
------------------------------------------------------------------------------------------------------------------------------------------------------------------
  @print_output                   bit       = 1,    -- Выводить ли результат принтами или нет, 1 = выводить
  @scripts_separator              nvarchar(512) = '
',                                                  -- Разделитель между скриптами, использующийся для конкатенации @oo_script и/или @df_script, при условии, что они не null на входе
  @debug                          bit       = 0     -- Режим отладки процедуры с выводом дополнительной информации о процессе исполнения
as
------------------------------------------------------------------------------------------------------------------------------------------------------------------
  set nocount on
------------------------------------------------------------------------------------------------------------------------------------------------------------------
  declare
    @default_collation_name sysname = (select top (1) [collation_name] from sys.databases with(nolock) where [database_id] = db_id())
------------------------------------------------------------------------------------------------------------------------------------------------------------------
  begin try
------------------------------------------------------------------------------------------------------------------------------------------------------------------
    if @object_name_or_query is null or @object_name_or_query = ''
      raiserror('Необходимо указать название объекта (таблицы, представления, табличной функции, табличного типа), либо текст запроса!', 16, 2)

    if @oo_type is null and @df_type is null
      raiserror('Необходимо указать хотя бы один параметр @oo_type или @df_type со значением не null!', 16, 2)

    if @oo_field_type_name_def_max_len is null
      set @oo_field_type_name_def_max_len = len('varchar(8000) collate ' + @default_collation_name)
 
    declare
      @object_name      sysname = isnull(quotename(parsename(@object_name_or_query, 2)) + '.', '') + quotename(parsename(@object_name_or_query, 1));
 
    declare
      @object_id        int     = isnull
                                  (
                                    object_id(@object_name_or_query),
                                    -- Может это табличный тип?
                                    (
                                      select [type_table_object_id]
                                      from sys.table_types
                                      where [name] = parsename(@object_name, 1)
                                        and [schema_id] = schema_id(parsename(@object_name, 2)))
                                  );
    declare
      @object_type      char(2) = cast(objectpropertyex(@object_id, N'BaseType') as char(2)),
      @max_line_length  int,
      @script           nvarchar(max) = null;

    set @oo_name  = isnull(
                      @oo_name,
                      iif(
                        @oo_type = 'PT',
                        @object_name,
                        N'@' + replace(replace(replace(replace(replace(object_name(@object_id), N':', N'_'), N'/', N'_'), N' ', N'_'), N'(', N'_'), N')', N'_')
                      )
                    )
------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Если тип объекта не определен, пробуем заселектить датасет со вставкой в таблицу из @object_name_or_query и выполнить рекурсию
------------------------------------------------------------------------------------------------------------------------------------------------------------------
    if @object_id is null
    begin
------------------------------------------------------------------------------------------------------------------------------------------------------------------
      if @@trancount > 0
        raiserror('Выполнение скриптования запроса не предусмотрено в открытой транзакции!', 16, 2)

      begin try
        set @object_name_or_query = 'select * into [gen].[table_script_output] from (' + @object_name_or_query + ') [!*rowset*!]' -- Какое-нибудь уникальное имя алиаса :)
 
        begin tran
 
          exec(@object_name_or_query)
 
          exec [gen].[table_script]
            @object_name_or_query           = '[gen].[table_script_output]',

            @oo_type                        = @oo_type,
            @oo_field_name_max_len          = @oo_field_name_max_len,
            @oo_field_type_name_def_max_len = @oo_field_type_name_def_max_len,
            @oo_max_data_rows               = @oo_max_data_rows,
            @oo_ignore_computed_columns     = @oo_ignore_computed_columns,
            @oo_column_keys_to_table_keys   = @oo_column_keys_to_table_keys,
            @oo_script                      = @oo_script out,

            @df_comment_field_value_columns = @df_comment_field_value_columns,
            @df_field_value_column_max_len  = @df_field_value_column_max_len,
            @df_type                        = @df_type,
            @df_script                      = @df_script out,
            
            @print_output                   = @print_output,
            @scripts_separator              = @scripts_separator,
            @debug                          = @debug
 
        rollback -- Датасет селектится в физ таблицу, которую затем скриптуем, и тут откатываем транзакцию, в которой была создана эта физ таблица [gen].[table_script_output]
 
        return
 
      end try
      begin catch
        if xact_state() <> 0
          rollback
 
        if @object_name is not null
          raiserror('Объект с именем %s не найден!', 16, 2, @object_name)
        else
        begin
          exec [gen].[print] @object_name_or_query
 
          declare @err nvarchar(4000) = error_message()
 
          raiserror(@err, 16, 2)
        end
      end catch
    end
------------------------------------------------------------------------------------------------------------------------------------------------------------------ 
    if @oo_field_name_max_len is null
      set @oo_field_name_max_len = 65536
 
    if @df_field_value_column_max_len is null
      set @df_field_value_column_max_len = 65536
 
    if @oo_type not in ('TV', 'Me', 'PT')
      raiserror('Неверно задан параметр @oo_type!', 16, 2)
 
    if @df_type not in ('IM', 'I', 'M')
      raiserror('Неверно задан параметр @df_type!', 16, 2)
 
    if @object_type not in ('V', 'U', 'TF', 'IF', 'TT')
      raiserror('Тип объекта %s не поддерживается!', 16, 2, @object_type)
------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Считывание данных
------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- 1. Уникальные ключи таблицы:
    declare @keys table
    (
      [columns]           nvarchar(max)   not null,
      [index_name]        sysname         not null,
      [index_type]        char(2)         not null,
      [type_desc]         nvarchar(60)    not null,
      [column_id]         int                 null,
      [merge_expression]  nvarchar(max)       null
    )
 
    insert @keys
    select
      [columns],
      [index_name],
      [index_type],
      [type_desc],
      [column_id],
      [merge_expression]
    from
    (
      select
        [index]             = row_number() over (partition by string_agg([index_column_desc], ', ') order by 1/0),
        [columns]           = string_agg([index_column_desc], ', '),
        [index_name]        = [index_name],
        [index_type]        = [index_type],
        [type_desc]         = [type_desc],
        [column_id]         = case
                                when count([index_column]) = 1
                                  then max([column_id])
                              end,
        [merge_expression]  = case when [index_type] = 'PK' THEN string_agg(N's.' + [index_column] + N' = d.' + [index_column], N' and ') END
      from
      (
        select
          [index_name]        = i.[name],
          [index_column]      = quotename(c.[name]),
          [index_column_desc] = quotename(c.[name]) + case when ic.[is_descending_key] = 1 then N' desc' else N'' end,
          [index_type]        = iif(i.[is_primary_key] = 1, 'PK', 'UQ'),
          [type_desc]         = lower(i.[type_desc]),
          [column_id]         = ic.[column_id]
        from sys.index_columns ic with(nolock)
        inner join sys.indexes i with(nolock) on i.[index_id] = ic.[index_id] and i.[object_id] = ic.[object_id]
        inner join sys.columns c with(nolock) on c.[column_id] = ic.[column_id] and c.[object_id] = ic.[object_id]
        where ic.[object_id] = @object_id
          and i.[is_unique] = 1
          and i.[is_disabled] = 0
          and ic.[is_included_column] = 0
          and i.[has_filter] = 0
        order by ic.[index_id], ic.[key_ordinal]
        offset 0 rows
      ) q
      group by [index_name], [index_type], [type_desc]
    ) q
    where [index] = 1 -- Исключить индексы, которые повторяют поля
 
    if @debug = 1
      select "@keys" = null, * from @keys
------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Формирование скрипта табличной структуры (output object)
------------------------------------------------------------------------------------------------------------------------------------------------------------------
    if @oo_type is not null
    begin
------------------------------------------------------------------------------------------------------------------------------------------------------------------
      -- 2. Поля таблицы:
      select
        @script =
          string_agg
          (
            concat_ws
            (
              N' ',
              [gen].[fn_margin_right]([field_name], [field_name_max_len], @oo_field_name_max_len, default),
              [gen].[fn_margin_right]([type_name_and_definition], [type_name_and_definition_max_len], @oo_field_type_name_def_max_len, default),
              [nullable],
              [constraints]
            ),
        N',
  '
          )
      from
      (
        select
          [field_name]                        = [field_name],
          [field_name_max_len]                = max(len([field_name])) over (),
          [type_name_and_definition]          = concat_ws(N' ', [type_name], [definition]),
          [type_name_and_definition_max_len]  = max(len(concat_ws(N' ', [type_name], [definition]))) over (),
          [nullable]                          = [nullable],
          [constraints]                       = [constraints]
        from
        (
          select
            [field_name]  = quotename(c.[name]),
            [type_name]   = iif(
                              cc.[is_computed] = 1,
                              N'',
                              [gen].[fn_field_type_name_get](t.[name], t.[system_type_id], t.[user_type_id], c.[max_length], c.[precision], c.[scale])
                            ),
            [definition]  = nullif(concat_ws
                            (
                              N' ',
                              case
                                when c.[is_identity] = 1
                                  then N'/*identity(' + cast(ident_current(@object_name) as nvarchar) + N',' + cast(ident_incr(@object_name) as nvarchar) + N')*/'
                              end,
                              case
                                when c.[collation_name] is not null and c.[collation_name] <> @default_collation_name
                                  then N'collate ' + c.[collation_name]
                              end,
                              case
                                when cc.[is_computed] = 1
                                  then N'as ' + cc.[definition] + iif(cc.[is_persisted] = 1, N' persisted', N'')
                              end
                            ), ''),
            [nullable]    = case
                              when cc.[is_computed] = 1
                                then N''
                              when c.[is_nullable] = 1 and sum(iif(c.[is_nullable] = 0, 1, 0)) over () > 0
                                then N'    null'
                              when c.[is_nullable] = 1
                                then N'null'
                                else N'not null'
                            end,
            [constraints] = nullif(concat_ws
                            (
                              N' ',
                              N'check ' + ck_c.[definition],
                              N'default ' + dc.[definition],
                              case
                                when k.[index_type] = 'PK'
                                  then N'primary key' + IIF(k.[type_desc] = 'clustered', N' ' + k.[type_desc], N'')
                                when k.[index_type] = 'UQ'
                                  then N'unique' + IIF(k.[type_desc] = 'clustered', N' ' + k.[type_desc], N'')
                              end
                            ), '')
          from sys.columns c with(nolock)
          inner join sys.types t with(nolock) on t.[system_type_id] = c.[system_type_id] and t.[user_type_id] = c.[user_type_id]
          left join sys.computed_columns cc with(nolock) on cc.[object_id] = @object_id and cc.[column_id] = c.[column_id]
          left join sys.default_constraints dc with(nolock) on dc.[parent_object_id] = @object_id and dc.[parent_column_id] = c.[column_id]
          left join sys.check_constraints ck_c with(nolock) on ck_c.[parent_object_id] = @object_id and ck_c.[parent_column_id] = c.[column_id]
          left join @keys k on k.[column_id] = c.[column_id] and @oo_column_keys_to_table_keys = 0
          where c.[object_id] = @object_id
            and (@oo_ignore_computed_columns = 0 or cc.[is_computed] is null or cc.[is_computed] = 0)
        ) x
      ) y
------------------------------------------------------------------------------------------------------------------------------------------------------------------
      -- 3. Получить уникальные ограничения (уникальные ключи и индексы будем рассматривать едино как уникальные ограничения для упрощения)
      select
        @script +=
          isnull
          (
            N',
  ' +       nullif
            (
              string_agg
              (
                iif([index_type] = 'PK', N'primary key', 'unique') +
                iif([type_desc] = 'clustered', N' ' + [type_desc], N'') +
                N' (' + [columns] + N')',
                N',
  '           ),
              N''
            ),
            N''
          )
      from
      (
        select *
        from @keys
        where [column_id] is null or @oo_column_keys_to_table_keys = 1
        order by iif([index_type] = 'PK', 0, 1)
        offset 0 rows
      ) q
------------------------------------------------------------------------------------------------------------------------------------------------------------------
      -- 4. Получить проверочные ограничения уровня таблицы:
      select
        @script +=
          isnull
          (
            N',
    ' +     nullif
            (
              string_agg
              (
                N'check ' + [definition],
                N',
    '         ),
              N''
            ),
            N''
          )
      from sys.check_constraints
      where [parent_object_id] = @object_id and [parent_column_id] = 0
------------------------------------------------------------------------------------------------------------------------------------------------------------------
      -- 5. Запрос создания таблицы:
      set @oo_script = concat_ws(
        @scripts_separator,
        @oo_script,
        iif(@oo_type = 'Me', N'  ' + @script, N'-- Скрипт ' +
        case @object_type
          when 'V'  THEN 'представления'
          when 'U'  THEN 'таблицы'
          when 'TF' THEN 'табличной функции'
          when 'IF' THEN 'inline-функции'
          when 'TT' THEN 'табличного типа'
        end + N' ' + @object_name +
        case
          when @object_type = 'TT' then N'
create type ' + @object_name + N' as table'
          when @oo_type = 'PT' then N'
create table ' + @oo_name
          when @oo_type = 'TV' then N'
declare ' + @oo_name + N' table'
        end + N'
(
  ' + @script + N'
)')
    )
------------------------------------------------------------------------------------------------------------------------------------------------------------------
    end
------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Формирование скрипта заполнения данными (data fill)
------------------------------------------------------------------------------------------------------------------------------------------------------------------ 
    if @object_type <> 'TT' and @df_type is not null
    begin
------------------------------------------------------------------------------------------------------------------------------------------------------------------
      -- 6. Запрос на получение данных для наполнения таблицы:
      -- ПРИМЕЧАНИЕ: %1001 - каждая тысячепервая запись будет резать наш скрипт на части по 1000 записей
      declare
        @has_datetime_col bit
      select
        @has_datetime_col = max(iif([type_name] in ('date', 'time', 'datetime2', 'datetimeoffset', 'smalldatetime', 'datetime'), 1, 0)),
        @script = N'
select
  @max_line_length =
    max
    (
      len(' +  +
      string_agg
      (
        cast(quotename([name]) + iif([is_last] = 1, N'', N' + N'', ''') +
    N' + isnull(replicate('' '', iif(@df_field_value_column_max_len < ' + quotename([name] + '(max_len)') + N', @df_field_value_column_max_len, ' + quotename([name] + '(max_len)') +
    N') - len(' + quotename([name]) + N')), N'''')' as nvarchar(max))
        , N' +
      '
      ) + N') + 3
    ),
  @script = N''' + IIF(@df_comment_field_value_columns = 1, N'--', N'  ') + N'('' +
    string_agg
    (
      iif([%1001] = 1, ''<!$%!>)
  ('', N'''') +
       ' +
      string_agg
      (
        cast(quotename([name]) + iif([is_last] = 1, N'', N' + N'', ''') +
    N' + isnull(replicate('' '', iif(@df_field_value_column_max_len < ' + quotename([name] + '(max_len)') + N', @df_field_value_column_max_len, ' + quotename([name] + '(max_len)') +
    N') - len(' + quotename([name]) + N')), N'''')' as nvarchar(max))
        , N' +
      '
      ) + N'
      , + N''),
  (''
     ) + N'')''
from
(
  select
    ' + iif(@df_type in ('I', 'IM'), N'[%1001],
    ', N'') +
        string_agg
        (
          cast(quotename([name]) + N',
    ' + quotename([name] + '(max_len)') + N' = max(isnull(len(' + quotename([name]) + N'), 0)) over ()' as nvarchar(max))
          , N',
    '   ) + N'
  from
  (' + iif(@df_comment_field_value_columns = 1, N'
    select
      ' + iif(@df_type in ('I', 'IM'), N'[%1001] = cast(null as bit),
      ', N'') +
          string_agg
          (
            cast(
              quotename([name]) + N' = ' +
              case
                when [type_name] = 'sql_variant'
                  then N'N''' + [name] + N''''
                else N'''' + [name] + N''''
              end as nvarchar(max))
            , N',
      '
          ) + N'
    union all', '') + N'
    select top (@oo_max_data_rows)
      ' + iif(@df_type in ('I', 'IM'), N'[%1001] = iif(row_number() over (order by 1/0) % 1001 = 0, cast(1 as bit), cast(0 as bit)),
      ', N'') +
          string_agg
          (
            cast(
              quotename([name]) + N' = ' +
              case
                when [type_name] in ('date', 'time', 'datetime2', 'datetimeoffset', 'smalldatetime', 'datetime')
                  then N'isnull('''''''' + replace(convert(nvarchar(max), ' + quotename([name]) + N', 121), '''''''', '''''''''''') + '''''''', ''null'')'
                when [type_name] IN ('text', 'uniqueidentifier', 'varchar', 'char')
                  then N'isnull('''''''' + replace(replace(replace(replace(replace(cast(' + quotename([name]) + N' as nvarchar(max)), '''''''', ''''''''''''), char(13), '''''' + char(13) + ''''''), char(10), '''''' + char(10) + ''''''), char(9), '''''' + char(9) + ''''''), '' + '''''''' + '', '' + '') + '''''''', ''null'')'
                when [type_name] IN ('ntext', 'nvarchar', 'nchar', 'xml', 'sysname')
                  then N'isnull(''N'' + '''''''' + replace(replace(replace(replace(replace(cast(' + quotename([name]) + N' as nvarchar(max)), '''''''', ''''''''''''), nchar(13), '''''' + nchar(13) + ''''''), nchar(10), '''''' + nchar(10) + ''''''), nchar(9), '''''' + nchar(9) + ''''''), '' + '''''''' + '', '' + '') + '''''''', ''null'')'
                when [type_name] IN ('image', 'hierarchyid', 'geometry', 'geography', 'varbinary', 'binary', 'timestamp')
                  then N'isnull(convert(nvarchar(max), cast(' + quotename([name]) + N' as varbinary(max)), 1), ''null'')'
                when [type_id] = 240 -- CLR-объект
                  then N'
        case
          when try_cast(' + quotename([name]) + N' as nvarchar(max)) is not null
            then ''N'' + '''''''' + replace(try_cast(' + quotename([name]) + N' as nvarchar(max)), '''''''', '''''''''''') + ''''''''
            else isnull(convert(nvarchar(max), cast(' + quotename([name]) + N' as varbinary(max)), 1), ''null'')
        end'
                when [type_name] = 'sql_variant'
                  then N'
        iif(row_number() over (order by 1/0) = 1, N''cast('', N'''')
        +
        case
          when sql_variant_property(' + quotename([name]) + N', ''basetype'') in (''date'', ''time'', ''datetime2'', ''datetimeoffset'', ''smalldatetime'', ''datetime'')
            then isnull('''''''' + cast(replace(convert(nvarchar(max), ' + quotename([name]) + N', 121), '''''''', '''''''''''') as nvarchar(4000)) + '''''''', ''null'')
          when sql_variant_property(' + quotename([name]) + N', ''basetype'') in (''text'', ''uniqueidentifier'', ''varchar'', ''char'')
            then isnull('''''''' + cast(replace(replace(replace(replace(replace(cast(' + quotename([name]) + N' as nvarchar(max)), '''''''', ''''''''''''), char(13), '''''' + char(13) + ''''''), char(10), '''''' + char(10) + ''''''), char(9), '''''' + char(9) + ''''''), '' + '''''''' + '', '' + '') as varchar(8000)) + '''''''', ''null'')
          when sql_variant_property(' + quotename([name]) + N', ''basetype'') in (''ntext'', ''nvarchar'', ''nchar'', ''xml'', ''sysname'')
            then isnull('''''''' + cast(''N'' + replace(replace(replace(replace(replace(cast(' + quotename([name]) + N' as nvarchar(max)), '''''''', ''''''''''''), nchar(13), '''''' + nchar(13) + ''''''), nchar(10), '''''' + nchar(10) + ''''''), nchar(9), '''''' + nchar(9) + ''''''), '' + '''''''' + '', '' + '') as nvarchar(4000)) + '''''''', ''null'')
          when sql_variant_property(' + quotename([name]) + N', ''basetype'') in (''image'', ''hierarchyid'', ''geometry'', ''geography'', ''varbinary'', ''binary'', ''timestamp'')
            then isnull(convert(varchar(8000), cast(' + quotename([name]) + N' as varbinary(8000)), 1), ''null'')
            else isnull(cast(' + quotename([name]) + N' as nvarchar(4000)), ''null'')
        end
        +
        iif(row_number() over (order by 1/0) = 1, N'' as sql_variant)'', N'''')'
                  else N'isnull(cast(' + quotename([name]) + N' as nvarchar(max)), ''null'')'
              end as nvarchar(max))
              , N',
      ') + N'
    from ' + @object_name + N' with(nolock)
  ) q
) x'
      from
      (
        select
          [is_last]   = iif(c.[column_id] = last_value(c.[column_id]) over (order by c.[column_id] rows between unbounded preceding and unbounded following), 1, 0),
          [name]      = c.[name],
          [type_name] = isnull(st.[name], tp.[name]),
          [type_id]   = tp.[system_type_id]
        from sys.columns c with(nolock)
        inner join sys.types tp with(nolock) on tp.[system_type_id] = c.[system_type_id] and tp.[user_type_id] = c.[user_type_id]
        left join sys.types st with(nolock) on st.[system_type_id] = c.[system_type_id] and st.[user_type_id] = st.[system_type_id]
        left join sys.computed_columns cc with(nolock) on cc.[object_id] = @object_id and cc.[column_id] = c.[column_id]
        where c.[object_id] = @object_id
          and cc.[is_computed] is null
        order by c.[column_id]
        offset 0 rows
      ) q
 
      if @debug = 1
      begin
        print '----DEBUG----'
        print formatmessage('
declare
  @max_line_length                int,
  @script                         nvarchar(max),
  @oo_max_data_rows               int = %s,
  @df_field_value_column_max_len  int = %s', isnull(cast(@oo_max_data_rows as nvarchar), 'null'), isnull(cast(@df_field_value_column_max_len as nvarchar), 'null'))
        exec [gen].[print] @script
      end 
------------------------------------------------------------------------------------------------------------------------------------------------------------------
      -- 7. Исполнить запрос и получить данные в виде строки @script:
      exec sys.sp_executesql
        @stmt                           = @script,
        @params                         = N'@oo_max_data_rows int, @df_field_value_column_max_len int, @script nvarchar(max) out, @max_line_length int out',
        @oo_max_data_rows               = @oo_max_data_rows,
        @df_field_value_column_max_len  = @df_field_value_column_max_len,
        @script                         = @script out,
        @max_line_length                = @max_line_length out;
 
      if @debug = 1
      begin
        print formatmessage('--@max_line_length = %d', @max_line_length)
        print '-------------'
      end
------------------------------------------------------------------------------------------------------------------------------------------------------------------ 
      -- 8. Скрипт на заполнение таблицы:
      select
        @df_script = concat_ws(
          @scripts_separator,
          @df_script,
          case
            when @has_datetime_col = 1
              then 'set dateformat ''ymd'''
          end,
          case
            when @oo_type = 'Me' then -- При выводе метаданных передаем как есть
              @script
            when mc.[merge_expression] is not null and @df_type = 'M'
              then N'merge ' + @oo_name + N' d
using
(
  values
' + @script + N'
) s (' + string_agg(quotename([name]), N', ') + N')
on ' + mc.[merge_expression] + N'
when not matched by target then
  insert (' + string_agg(quotename([name]), N', ') + N')
  values (' + string_agg(quotename([name]), N', ') + N')
when matched then
  update set
    ' +       string_agg
              (
                case when [is_primary_key] = 0 then [gen].[fn_margin_right]('d.' + quotename([name]), [name_max_len] + 2, default, default) + N' = s.' + quotename([name]) end,
                N',
    '
              ) + N';'
            when mc.[merge_expression] is not null and @df_type = 'IM' and @oo_max_data_rows <= 1000
              then N'insert ' + @oo_name + N'
  (' + string_agg(quotename([name]), N', ') + N')
--merge ' + @oo_name + N' d
--using
--(
  values
' + @script + N'
--) s (' + string_agg(quotename([name]), N', ') + N')
--on ' + mc.[merge_expression] + N'
--when not matched by target then
--  insert (' + string_agg(quotename([name]), N', ') + N')
--  values (' + string_agg(quotename([name]), N', ') + N')
--when matched then
--  update set
--    ' +     string_agg
              (
                case when [is_primary_key] = 0 then [gen].[fn_margin_right]('d.' + quotename([name]), [name_max_len] + 2, default, default) + N' = s.' + quotename([name]) end,
            N',
--    '
              ) + N';'
              else
          N'insert ' + @oo_name + N'
  (' + string_agg(quotename([name]), N', ') + N')
values
' + replace(@script, N',
  (<!$%!>)', N';

insert ' + @oo_name + N'
  (' + string_agg(quotename([name]), N', ') + N')
values') + N''
          end
        )
      from
      (
        select
          [is_last]         = iif(c.[column_id] = last_value(c.[column_id]) over (order by c.[column_id] rows between unbounded preceding and unbounded following), 1, 0),
          [name]            = c.[name],
          [name_max_len]    = max(len(quotename(c.[name]))) over (partition by nullif(v.[is_primary_key], 0)),
          [type_name]       = tp.[name],
          [is_primary_key]  = v.[is_primary_key]
        from sys.columns c with(nolock)
        inner join sys.types tp with(nolock) on tp.[system_type_id] = c.[system_type_id] and tp.[user_type_id] = c.[user_type_id]
        left join sys.computed_columns cc with(nolock) on cc.[object_id] = @object_id and cc.[column_id] = c.[column_id]
        outer apply
        (
          select
            [is_primary_key] =
              case
                when
                  exists
                  (
                    select *
                    from sys.index_columns ic with(nolock)
                    inner join sys.indexes i with(nolock) on i.[index_id] = ic.[index_id] and i.[object_id] = ic.[object_id] and i.[is_unique] = 1
                    where ic.[column_id] = c.[column_id]
                      and ic.[object_id] = c.[object_id]
                      and ic.[is_included_column] = 0
                      and i.[is_primary_key] = 1
                  )
                  then 1
                  else 0
              end
        ) v
        where c.[object_id] = @object_id
          and cc.[is_computed] is null
        order by c.[column_id]
        offset 0 rows
      ) q
      outer apply
      (
        select top (1)
          [merge_expression],
          [columns]
        from @keys
        where [merge_expression] is not null
      ) mc
      group by [merge_expression]
------------------------------------------------------------------------------------------------------------------------------------------------------------------
    end
------------------------------------------------------------------------------------------------------------------------------------------------------------------ 
    set @script = concat_ws(@scripts_separator, @oo_script, @df_script)
 
    if @print_output = 1
      -- если максимальная длина строк получилась 2000 и выше, то командой print вывести ее не сможем, используем вывод в виде xml:
      if @max_line_length > 4000
        select [script] = (select [processing-instruction(x)] = @script for xml path(''), type)
      else
        exec [gen].[print] @script 
------------------------------------------------------------------------------------------------------------------------------------------------------------------
  end try
------------------------------------------------------------------------------------------------------------------------------------------------------------------
  begin catch
------------------------------------------------------------------------------------------------------------------------------------------------------------------
    throw;    
------------------------------------------------------------------------------------------------------------------------------------------------------------------
  end catch
------------------------------------------------------------------------------------------------------------------------------------------------------------------
