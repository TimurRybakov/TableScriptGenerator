# TableScriptGenerator

A procedure [gen].[table_script] with subroutones to generate scripts of table variable declaration upon tables, views, table functions and some select statements for the current database. Data fill may or may not be included depending on @df_type parameter value (included by default).

This procedure is a tool that helps to write or rewrite sql statements, gerenate data filling scripts.

```sql
EXEC [gen].[table_script] @object_name_or_query = '<OBJECT NAME>'
```

## Examples

Suggest there is a table:

```sql
create table [Test]
(
  [Id]    int not null primary key clustered,
  [Value] nvarchar(500) null
)

insert [Test] values (1, 'One'), (2, 'Two'), (3, 'Three')
```

## #1 execution
```sql
exec [gen].[table_script] 'select * from Test where Id < 3'
```
> output
> ```sql
> -- Скрипт таблицы [gen].[table_script_output]
> declare @table_script_output table table
> (
>   [Id]    int           not null,
>   [Value] nvarchar(500)     null
> )
> insert @table_script_output
>   ([Id], [Value])
> values
> --(Id, Value   ),
>   (1,  N'One'  ),
>   (2,  N'Two'  )
> ```

## #2 execution
```sql
exec [gen].[table_script] 'Test'
```
> output
> ```sql
> -- Скрипт таблицы [Test]
> declare @Test table
> (
>   [Id]    int           not null primary key clustered,
>   [Value] nvarchar(500)     null
> )
> merge @Test d
> using
> (
>   values
> --(Id, Value   ),
>   (1,  N'One'  ),
>   (2,  N'Two'  ),
>   (3,  N'Three')
> ) s ([Id], [Value])
> on s.[Id] = d.[Id]
> when not matched by target then
>   insert ([Id], [Value])
>   values ([Id], [Value])
> when matched then
>   update set
>     d.[Value] = s.[Value];
> ```

# Installation

You may use "single script\table script generator.sql" script file for fast deployment.
