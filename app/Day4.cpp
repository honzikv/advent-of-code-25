//go:build ignore

#include <iostream>
#include <string>
#include <vector>
#include <filesystem>
#include <fstream>
#include <algorithm>

using Grid = std::vector<std::vector<char>>;
using ForkliftableIndices = std::vector<std::pair<uint32_t, uint32_t>>;

std::vector<char> getNeighbors(const Grid &grid, uint32_t i, uint32_t j);
std::vector<std::pair<uint32_t, uint32_t>> getForkliftableRollIndices(const Grid &grid, uint32_t maxNeighborRolls = 3);
Grid forkliftIndices(const Grid &grid, const ForkliftableIndices &forkliftableIndices);

int main(int argc, char *argv[])
{
    std::cout << "Day 4️⃣" << std::endl;
    if (argc != 2)
    {
        std::cerr << "Oh fuck off." << std::endl;
        exit(1);
    }

    // Read the file
    const std::filesystem::path filePath = argv[1];
    std::vector<std::string> lines;
    std::string currentLine;

    {
        std::ifstream inputFileStream(filePath);
        if (!inputFileStream.is_open())
        {
            std::cerr << "Fuck off." << std::endl;
            exit(1);
        }
        while (std::getline(inputFileStream, currentLine))
        {
            lines.push_back(currentLine);
        }
    }

    // Create the "matrix"
    Grid grid;
    std::transform(
        lines.begin(),
        lines.end(),
        std::back_inserter(grid),
        [](const std::string &line)
        {
            return std::vector<char>(line.begin(), line.end());
        });

    auto rollsRemoved = 0;
    while (true)
    {
        const auto forklifableIndices = getForkliftableRollIndices(grid);
        if (forklifableIndices.empty())
        {
            break;
        }

        rollsRemoved += forklifableIndices.size();
        grid = forkliftIndices(grid, forklifableIndices);
    }

    std::cout << rollsRemoved << std::endl;
}

ForkliftableIndices getForkliftableRollIndices(const Grid &grid, uint32_t maxNeighborRolls)
{
    ForkliftableIndices result;
    for (auto i = 0u; i < grid.size(); i += 1)
    {
        for (auto j = 0u; j < grid[i].size(); j += 1)
        {
            const std::vector<char> neighbors = getNeighbors(grid, i, j);
            const auto neighborRolls = std::count_if(
                neighbors.begin(),
                neighbors.end(),
                [](const char &neighbor)
                { return neighbor == '@'; });

            if (neighborRolls <= maxNeighborRolls && grid[i][j] == '@')
            {
                result.push_back({i, j});
            }
        }
    }

    return result;
}

Grid forkliftIndices(const Grid &grid, const ForkliftableIndices &forkliftableIndices)
{
    auto result = Grid(grid);
    for (const auto [i, j] : forkliftableIndices)
    {
        result[i][j] = '.';
    }

    return result;
}

std::vector<char> getNeighbors(const Grid &grid, uint32_t i, uint32_t j)
{
    std::vector<char> result;
    const auto dim = grid[0].size();
    const auto rows = grid.size();
    const auto cols = grid[0].size();
    if (i + 1 < rows)
    {
        result.push_back(grid[i + 1][j]);

        if (j + 1 < cols)
        {
            result.push_back(grid[i + 1][j + 1]);
        }
        if (j > 0)
        {
            result.push_back(grid[i + 1][j - 1]);
        }
    }

    if (i > 0)
    {
        result.push_back(grid[i - 1][j]);

        if (j > 0)
        {
            result.push_back(grid[i - 1][j - 1]);
        }
        if (j + 1 < cols)
        {
            result.push_back(grid[i - 1][j + 1]);
        }
    }

    if (j + 1 < cols)
    {
        result.push_back(grid[i][j + 1]);
    }

    if (j > 0)
    {
        result.push_back(grid[i][j - 1]);
    }

    return result;
}