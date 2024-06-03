#include <chrono>
#include <cmath>
#include <cstdint>
#include <iostream>
#include <numeric>
#include <span>
#include <optional>
#include <vector>

#include "SIMDHelpers.h"

struct InlineItem {
    enum class Type {
        whitespace,
        nonWhitespace,
    };

    bool operator==(const InlineItem &rhs) const
    {
        return position == rhs.position && length == rhs.length && type == rhs.type;
    }
    size_t position;
    size_t length;
    Type type;
};

struct Content {
    size_t length;
};

bool isWhitespace(char character)
{
    return character == ' ' || character == '\t' || character == '\n';
}

std::optional<Content> moveToNextNonWhitespacePosition(size_t position, const std::string &text, bool optimize)
{
    constexpr auto charactersPerRegister = sizeof(decltype(splat(' '))) / sizeof(char);
    constexpr auto newlineMask = splat('\n');
    constexpr auto spaceMask = splat(' ');
    constexpr auto tabMask = splat('\t');

    size_t end = position;
    while (end < text.size()) {
        const bool useIndividualCharacterComparison = !optimize || text.size() - end < charactersPerRegister;
        if (useIndividualCharacterComparison) {
            while (end < text.size() && isWhitespace(text[end]))
                ++end;
            break;
        } else {
            const auto characterVector = load(reinterpret_cast<const uint8_t *>(text.data()) + end);
            auto whitespaceMatches = merge(
                equal(characterVector, newlineMask),
                equal(characterVector, spaceMask),
                equal(characterVector, tabMask));
            const auto nonWhitespaceMatches = bitNot(whitespaceMatches);
            auto lane = findFirstNonZeroIndex(nonWhitespaceMatches);
            if (!lane) {
                end += charactersPerRegister;
            } else {
                end += lane.value();
                break;
            }
        }
    }
    return end == position ? std::nullopt : std::make_optional(Content {end - position});
}

std::optional<Content> moveToNextWhitespacePosition(size_t position, const std::string &text, bool optimize)
{
    constexpr auto charactersPerRegister = sizeof(decltype(splat(' '))) / sizeof(char);
    constexpr auto newlineMask = splat('\n');
    constexpr auto spaceMask = splat(' ');
    constexpr auto tabMask = splat('\t');

    size_t end = position;
    while (end < text.size()) {
        const bool useIndividualCharacterComparison = !optimize || text.size() - end < charactersPerRegister;
        if (useIndividualCharacterComparison) {
            while (end < text.size() && !isWhitespace(text[end]))
                ++end;
            break;
        }
        else {
            const auto characterVector = load(reinterpret_cast<const uint8_t *>(text.data()) + end);
            auto whitespaceMatches = merge(
                equal(characterVector, newlineMask),
                equal(characterVector, spaceMask),
                equal(characterVector, tabMask));
            auto lane = findFirstNonZeroIndex(whitespaceMatches);
            if (!lane) {
                end += charactersPerRegister;
            } else {
                end += lane.value();
                break;
            }
        }
    }
    return end == position ? std::nullopt : std::make_optional(Content {end - position});
}

void processText(std::vector<InlineItem> &inlineItems, const std::string &text, bool optimize = false)
{
    if (text.empty())
        return;
    size_t position = 0;
    while (position < text.size())
    {
        auto handleWhitespace = [&] {
            auto whitespaceContent = moveToNextNonWhitespacePosition(position, text, optimize);
            if (!whitespaceContent)
                return false;
            inlineItems.push_back(InlineItem{position, whitespaceContent->length, InlineItem::Type::whitespace});
            position += whitespaceContent->length;
            return true;
        };
        if (handleWhitespace())
            continue;

        auto handleNonWhitespace = [&] {
            auto nonWhitespaceContent = moveToNextWhitespacePosition(position, text, optimize);
            if (!nonWhitespaceContent)
                return false;
            inlineItems.push_back(InlineItem{position, nonWhitespaceContent->length, InlineItem::Type::nonWhitespace});
            position += nonWhitespaceContent->length;
            return true;
        };
        if (handleNonWhitespace())
            continue;
    };
}

class Timer {
private:
    using clock_t = std::chrono::high_resolution_clock;
    using microseconds = std::chrono::microseconds;
    std::chrono::time_point<clock_t> m_start;

public:
    Timer() : m_start(clock_t::now()) {}
    void reset() { m_start = clock_t::now(); }
    long long elapsed() const
    {
        return std::chrono::duration_cast<microseconds>(clock_t::now() - m_start).count();
    }
};

// Benchmark function
std::tuple<double, double, int> benchmark(const std::string &text, bool optimize)
{
    constexpr int num_iterations = 10000; // Adjust as needed
    std::vector<double> timings(num_iterations);

    size_t sideEffect = 0;
    for (int i = 0; i < num_iterations; ++i) {
        Timer timer;
        std::vector<InlineItem> inlineItems;
        processText(inlineItems, text, optimize);
        sideEffect += inlineItems.size();
        timings[i] = timer.elapsed() / 1000.0; // Convert to milliseconds
    }

    // side effect so code is not optimized out
    std::cerr << "(ignore the following number): " << sideEffect << std::endl;

    // Calculate statistics
    double sum = std::accumulate(timings.begin(), timings.end(), 0.0);
    double mean = sum / num_iterations;

    // Calculate confidence interval (assuming normal distribution)
    double variance = 0.0;
    for (double timing : timings) {
        variance += (timing - mean) * (timing - mean);
    }
    variance /= num_iterations;
    double stddev = std::sqrt(variance);

    return std::make_tuple(mean, stddev, num_iterations);
}

// approximation of CDF for students t distribution
double studentsTCdf(double t, int df)
{
    if (t < 0)
        return 1 - studentsTCdf(-t, df);
    double p = 0.5;
    double a = t / sqrt(df);
    double b = df / (df + a * a);
    double c = p * b;
    double d = 1 - c;
    double s = a * sqrt(b);
    double z = s * (0.25 * c + 0.5 * d - c * d);
    double x = z / sqrt(2 * M_PI);
    double y = 0;
    for (int i = 0; i <= df - 1; i += 2) {
        y += x;
        a = a * (i / 2 + 0.5) / (i + 1);
        b *= c;
        d *= -c;
        s -= 1;
        x *= s * (b + d);
    }
    return 0.5 + y;
}

int main()
{
    std::string text = "The quick brown fox jumped over the lazy dog. ASLDH AKWJD LAKJSHD KAJSHDK LAJHSLDKH AJSDJ HKJKHKAJSHD LKAHWD UIQHPWU HDJK KNcckjzsdn lajhd qipuwj aklsl kJNASlkjd NALjh fqupiwh ajklsd JKASjdk HALd Qhuwdh ashdk jakjda jfkhq. AIWDUH\n LKAJWHDKLAJS LKJAWD ASNJA KSD KLJANWLKD JNQWKLJR HAUS OIAHD POQWHD KJASND LQJKWBD LIQBSDU IAHSDKJ NQLWE LAJSHDB LJASND :KAW DIAJS :KLAJW: OAIWJDO QUBGHJKLSBDJKNALSKJ dn sdfhlkjwdk jqwhd iuqhwdiuqhdiush dkjskd zjskdf hiuqw hrqpwiu doisjd copijqw doiqjw doKAJS djlsfkj awekf hasdf akjsdfjaklwnef lkjawdfkj asdkjf askjdfkjasdfkajsdfjkaskdfh awieufh asiudjf ljakwehf ajwehfu piasdhgi afhdgkjahjrkeghwai uefh aiwdhql kwjeh qowu opuasigo uasodiu pfaowief oaiwefjawskdjsdbfjk asbdnfawnmefbawjkefkjsdfhiuaerhgiuh sdjkvn akjsdfn aw,emnbf akwjsdshui hapoweuhf kajwebfd lkbqawkjqkjweqkjwh ;iuhwofpiau weoi hawjrh 983yr p9283hf iudjnf ljk2n3r jwdnfajsd'0[a923t[ 09awjeo;fiawen;uifh2p398rh p9a8wehf;i ua23r ha239p8r haw9e8fu23hr1hr2082u3 t8hadjf n;awkefj 0248u p0a348th aow;uef;n ua23hr08 ahweifh wo;fa0238rhhk   .";

    auto [meanNonOptimized, stddevNonOptimized, sampleSizeNonOptimized] = benchmark(text, false);
    auto [meanOptimized, stddevOptimized, sampleSizeOptimized] = benchmark(text, true);

    double pooledStddev = std::sqrt((stddevNonOptimized * stddevNonOptimized + stddevOptimized * stddevOptimized) / (sampleSizeNonOptimized + sampleSizeOptimized - 2));
    double tStat = (meanOptimized - meanNonOptimized) / (pooledStddev * std::sqrt(1.0 / sampleSizeNonOptimized + 1.0 / sampleSizeOptimized));

    std::cout << "\nNon-Optimized Version:" << std::endl;
    std::cout << "Mean: " << meanNonOptimized << " ms" << std::endl;
    std::cout << "Standard Deviation: " << stddevNonOptimized << " ms" << std::endl;
    std::cout << "Sample Size: " << sampleSizeNonOptimized << std::endl;

    std::cout << "\nOptimized Version:" << std::endl;
    std::cout << "Mean: " << meanOptimized << " ms" << std::endl;
    std::cout << "Standard Deviation: " << stddevOptimized << " ms" << std::endl;
    std::cout << "Sample Size: " << sampleSizeOptimized << std::endl;

    int degreesOfFreedom = sampleSizeNonOptimized + sampleSizeOptimized - 2;

    // Calculate p-value
    double pValue;
    if (tStat > 0)
        pValue = 1 - studentsTCdf(degreesOfFreedom / 2, tStat * tStat / 2);
    else
        pValue = studentsTCdf(degreesOfFreedom / 2, tStat * tStat / 2);

    // Output results
    std::cout << "\np-value for the optimized version being faster: " << pValue << std::endl;

    return 0;
}
