/*
Возвращает строку @value, дополненную отступами @margin до длины @len, но не более @max_len

select [gen].[fn_margin_right]('abc', 10, 4, N'*')

*/
create   function [gen].[fn_margin_right]
(
  @value    nvarchar(512),
  @len      int,
  @max_len  int  = null,
  @margin   nchar(1)  = N' '
)
returns nvarchar(max)
as
begin
  return
  (
    @value +
    isnull
    (
      replicate
      (
        @margin,
        iif(@max_len < @len, @max_len, @len) - len(@value)
      ),
      N''
    )
  )
end
