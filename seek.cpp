#include <algorithm>
#include <vector>
#include <fstream>
#include <chrono>
#include <ctime>
#include <numeric>
#include <iostream>

char readBytes(std::ifstream& stream, int offset)
{
    char buffer[128];
    stream.seekg(offset, stream.beg);
    stream.read(buffer, 128);
    return buffer[0];
}

volatile char sink;

int main()
{
    std::vector<double> elapsed;

    for(auto iter = 0; iter < 100; ++iter)
    {
        std::chrono::high_resolution_clock::time_point start(std::chrono::high_resolution_clock::now());
        std::ifstream is("data.txt", std::ifstream::binary);

        if (is)
        {
            std::vector<int> v(100000);
            int n = 0;

            generate(v.begin(), v.end(), [&n] { return n++ * 100;});

            for(auto i : v)
            {
                sink = readBytes(is, i);
            }
        }

        is.close();

        std::chrono::duration<double> diff = std::chrono::high_resolution_clock::now() - start;
        elapsed.push_back(diff.count());
    }

    auto min = *std::min_element(elapsed.begin(), elapsed.end());
    auto max = *std::max_element(elapsed.begin(), elapsed.end());
    auto avg = std::accumulate( elapsed.begin(), elapsed.end(), 0.0) / elapsed.size(); 

    std::cout << "Avg: " << avg * 1000 << " ms | Min: " << min * 1000 << " ms | Max: " << max * 1000 << " ms" << std::endl;
}
