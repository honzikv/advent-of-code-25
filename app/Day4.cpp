#include <iostream>
#include <string>
#include <vector>
#include <filesystem>
#include <fstream>
#include <algorithm>

std::vector<char> getNeighbors(const std::vector<std::vector<char>> &grid, size_t i, size_t j);
uint64_t getTotalForkliftableRolls(const std::vector<std::vector<char>> &grid, size_t maxNeighborRolls = 3);


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
    std::vector<std::vector<char>> grid;
    std::transform(
        lines.begin(),
        lines.end(),
        std::back_inserter(grid),
        [](const std::string &line)
        {
            return std::vector<char>(line.begin(), line.end());
        });

    std::cout << "total of " << lines.size() << " lines" << std::endl;
    const auto total = getTotalForkliftableRolls(grid);
    std::cout << total << std::endl;
}

uint64_t getTotalForkliftableRolls(const std::vector<std::vector<char>> &grid, size_t maxNeighborRolls)
{
    size_t count = 0;
    std::cout << "grid size X=" << grid.size() << "Y=" << grid[0].size() << std::endl;
    for (auto i = 0; i < grid.size(); i += 1)
    {
        for (auto j = 0; j < grid[i].size(); j += 1)
        {
            const std::vector<char> neighbors = getNeighbors(grid, i, j);
            const size_t neighborRolls = std::count_if(
                neighbors.begin(),
                neighbors.end(),
                [](const char &neighbor)
                { return neighbor == '@'; });
            // std::cout << "total of " << neighbors.size() << " neighbors" << std::endl;

            if (neighborRolls <= maxNeighborRolls && grid[i][j] == '@') {
                count += 1;
                std::cout << "[" << i << "," << j << "]" << std::endl;
            }
        }
    }

    return count;
}

std::vector<char> getNeighbors(const std::vector<std::vector<char>> &grid, size_t i, size_t j)
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