defmodule Day6 do
  def main do
    IO.puts("Day 6")

    if System.argv() |> length() != 1 do
      raise "Invalid args"
    end

    file_path = hd(System.argv())
    IO.inspect(file_path, label: "file_path")

    case File.read(file_path) do
      {:ok, content} ->
        lines = content |> String.split("\n")
        extract_celaphopod_exprs(lines) |> compute_cephalopod_exprs() |> IO.puts()
        extract_transposed_cephalopod_exprs(lines) |> compute_cephalopod_exprs() |> IO.puts()
    end
  end

  defp part2(lines) do
  end

  defp extract_celaphopod_exprs(lines, expression_rows \\ []) do
    case lines do
      [] ->
        expression_rows

      [last_line] ->
        ops = last_line |> String.split(" ", trim: true)
        # Transpose the result so instead of [[numbers row 1], ... [+ * + *...]] we get
        # [+ numbers 1], [* numbers 2], ... where each element is one math calculation
        [ops | expression_rows] |> Enum.zip() |> Enum.map(&Tuple.to_list/1)

      [line | remaining_lines] ->
        expression_row =
          line
          |> String.split(" ", trim: true)
          |> Enum.map(fn x -> String.to_integer(x) end)

        extract_celaphopod_exprs(remaining_lines, [expression_row | expression_rows])
    end
  end

  defp compute_cephalopod_exprs(math_exprs) do
    math_exprs
    |> Enum.map(fn [op | terms] ->
      case op do
        "+" -> terms |> Enum.sum()
        "*" -> terms |> Enum.reduce(1, fn x, a -> x * a end)
      end
    end)
    |> Enum.sum()
  end

  defp read_columns("", _column_widths, values), do: values |> Enum.reverse()
  defp read_columns(_lines, [], values), do: values |> Enum.reverse()

  defp read_columns(line, column_widths, values) do
    [split_at | remaining_column_widths] = column_widths
    {value, remaining_line_contents} = line |> String.split_at(split_at)
    updated_values = [value | values]
    # This replaces the first space with nothing, as all columns are separated by space
    {_, remaining_line_contents} = remaining_line_contents |> String.split_at(1)
    read_columns(remaining_line_contents, remaining_column_widths, updated_values)
  end

  defp extract_transposed_cephalopod_exprs(lines) do
    # For each column, get the column width
    column_widths =
      lines
      # 123 328  51 64
      # 45 64  387 23
      # 6 98  215 314
      # *   +   *   +
      # e.g. [123, 328, 51, 64] - each column has one term from different math equation
      |> Enum.map(fn line -> String.split(line, " ", trim: true) end)
      # Transpose - now each row contains grouped math expressions {123, 45, 6}
      |> Enum.zip()
      # Get maximum string length for each math expression, so we can easily read the lines
      |> Enum.map(fn col_values ->
        col_values
        |> Tuple.to_list()
        |> Enum.max_by(fn row -> String.length(row) end)
        |> String.length()
      end)

    # Now read input, e.g. if we know that for the first column the maximum string length is 3,
    # we read first three chars, skip one (it's a space) and then can move to the next equation
    lines
    |> Enum.map(fn line -> read_columns(line, column_widths, []) end)
    # Now each row contains a list for one term in each equation, so transpose to get
    # all terms for one equation in a single row, so e.g. {"123", "45 ", "6  ", "*  "}
    |> Enum.zip()
    # Reverse each list so we can easily destructure operator
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.map(&Enum.reverse/1)
    |> Enum.map(fn math_expr ->
      [operator | terms] = math_expr

      transposed =
        terms
        # unreverse
        |> Enum.reverse()
        |> Enum.map(&String.codepoints/1)
        |> Enum.zip()
        |> Enum.map(fn char_tuple ->
          char_tuple
          |> Tuple.to_list()
          |> Enum.filter(fn x -> x != " " end)
          |> Enum.join()
          |> String.to_integer()
        end)

      operator = operator |> String.replace(" ", "", global: true)
      [operator | transposed]
    end)
  end
end

Day6.main()
