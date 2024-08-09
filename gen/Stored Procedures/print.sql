-- Вывод принтами строки любой длины с переносами
create   procedure [gen].[print]
  @msg nvarchar(max)
as
begin
  set nocount on;

  declare @msg_len int;
  declare @cur_line_start_idx int = 1;
  declare @cur_line_end_idx int;
  declare @cur_line_len int;
  declare @skip_count int;

  -- normalise line end characters.
  set @msg = replace(@msg, nchar(13) + nchar(10), '@#$');
  set @msg = replace(@msg, nchar(10) + nchar(13), '@#$');
  set @msg = replace(@msg, nchar(13), nchar(10));
  set @msg = replace(@msg, '@#$', nchar(10));

  -- store length of the normalised string.
  set @msg_len = len(@msg);

  -- special case: empty string.
  if @msg_len = 0
  begin
    print '';
    return;
  end

  -- find the end of next substring to print.
  set @cur_line_end_idx = charindex(nchar(10), @msg);
  if @cur_line_end_idx between 1 and 4000
  begin
    set @cur_line_end_idx -= 1
    set @skip_count = 2;
  end
  else
  begin
    set @cur_line_end_idx = 4000;
    set @skip_count = 1;
  end

  -- loop: print current substring, identify next substring (a do-while pattern is preferable but tsql doesn't have one).
  while @cur_line_start_idx < @msg_len
  begin
    -- print substring.
    print substring(@msg, @cur_line_start_idx, (@cur_line_end_idx - @cur_line_start_idx)+1);

    -- move to start of next substring.
    set @cur_line_start_idx = @cur_line_end_idx + @skip_count;

    -- find the end of next substring to print.
    set @cur_line_end_idx = charindex(nchar(10), @msg, @cur_line_start_idx);
    set @cur_line_len = @cur_line_end_idx - @cur_line_start_idx;

    -- find bounds of next substring to print.
    if @cur_line_len between 0 and 4000
    begin
      set @cur_line_end_idx -= 1

      set @skip_count = 2;
    end
    else
    begin
      set @cur_line_end_idx = @cur_line_start_idx + 4000;
      set @skip_count = 1;
    end
  end
end
