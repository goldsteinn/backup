#include <assert.h>
#include <immintrin.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <x86intrin.h>
#include <cstddef>
#include <type_traits>

#define ALWAYS_INLINE inline __attribute__((always_inline))
#define NEVER_INLINE  __attribute__((noinline))
#define CONST_ATTR    __attribute__((const))
#define PURE_ATTR     __attribute__((pure))

#define COMPILER_BARRIER() asm volatile("" : : : "memory");
#define COMPILER_DO_NOT_OPTIMIZE_OUT(X)                                        \
    asm volatile("" : : "i,r,m"(X) : "memory")

#define IMPOSSIBLE(X)                                                          \
    if (X) {                                                                   \
        __builtin_unreachable();                                               \
    }

#define PRINT(...)  // fprintf(stderr, __VA_ARGS__)

enum timer_conf { CYCLES = 0, GETTIME = 1 };
template<timer_conf conf = GETTIME>
struct timer {
    static constexpr clockid_t cid       = CLOCK_MONOTONIC;
    static constexpr uint64_t  sec_to_ns = 1000 * 1000 * 1000;

    using time_t = typename std::
        conditional_t<conf == timer_conf::CYCLES, uint64_t, struct timespec>;

    time_t tstart;
    time_t tend;

    const char * const
    units() const {
        if constexpr (conf == timer_conf::CYCLES) {
            return "cycles";
        }
        else {
            return "ns";
        }
    }

    void ALWAYS_INLINE
    start() {
        if constexpr (conf == timer_conf::CYCLES) {
            tstart = _rdtsc();
        }
        else {
            clock_gettime(cid, &tstart);
        }
    }

    void ALWAYS_INLINE
    end() {
        if constexpr (conf == timer_conf::CYCLES) {
            tend = _rdtsc();
        }
        else {
            clock_gettime(cid, &tend);
        }
    }

    uint64_t
    get_start() const {
        if constexpr (conf == timer_conf::CYCLES) {
            return tstart;
        }
        else {
            return sec_to_ns * tstart.tv_sec + tstart.tv_nsec;
        }
    }
    uint64_t
    get_end() const {
        if constexpr (conf == timer_conf::CYCLES) {
            return tend;
        }
        else {
            return sec_to_ns * tend.tv_sec + tend.tv_nsec;
        }
    }

    uint64_t
    dif() {
        return get_end() - get_start();
    }

    double
    ddif() {
        return ((double)dif());
    }

    double
    ddif(uint64_t n) {
        return ddif() / ((double)n);
    }

    void
    std_print() {
        std_print("", 1);
    }

    void
    std_print(const char * const hdr) {
        std_print(hdr, 1);
    }

    void
    std_print(uint64_t n) {
        std_print("", n);
    }

    void
    std_print(const char * const hdr, uint64_t n) {
        if (hdr[0] == 0) {
            fprintf(stderr, "%.3E %s\n", ddif(n), units());
        }
        else {
            fprintf(stderr, "%-8s: %.3E %s\n", hdr, ddif(n), units());
        }
    }
};

#define PRIMITIVE_CAT(x, y) x##y
#define CAT(x, y)           PRIMITIVE_CAT(x, y)

#define PRIMITIVE_V_TO_STR(X) #X
#define V_TO_STR(X)           PRIMITIVE_V_TO_STR(X)

#define PRINT_V(X)  // print_v<uint8_t>(X, V_TO_STR(X))

#define SIMD_I(instruction, ...)  CAT(SIMD, VSIZE)(instruction, __VA_ARGS__)
#define SIMD_IP(instruction, ...) CAT(SIMD, VSIZE)(instruction, __VA_ARGS__)

#define SIMD16(instruction, ...) CAT(_mm_, instruction)(__VA_ARGS__)
#define SIMD32(instruction, ...) CAT(_mm256_, instruction)(__VA_ARGS__)
#define SIMD64(instruction, ...) CAT(_mm512_, instruction)(__VA_ARGS__)

#define SIMDP16(instruction, ...) CAT(_mm_, instruction)(__VA_ARGS__)
#define SIMDP32(instruction, ...) CAT(_mm256_, instruction)(__VA_ARGS__)
#define SIMDP64(instruction, ...) CAT(_mm512_, instruction)(__VA_ARGS__)

template<typename T>
void
print_v(__m256i v, const char * const hdr) {
    T arr[sizeof(__m256i) / sizeof(T)];
    _mm256_storeu_si256((__m256i *)arr, v);
    fprintf(stderr, "%-8s: [%x", hdr, arr[0] & ((1u << (8 * sizeof(T))) - 1));
    for (uint32_t i = 1; i < sizeof(__m256i) / sizeof(T); ++i) {
        fprintf(stderr, ",%x", arr[i] & ((1u << (8 * sizeof(T))) - 1));
    }
    fprintf(stderr, "]\n");
}

template<typename T>
void
print_v(__m128i v, const char * const hdr) {
    T arr[sizeof(__m128i) / sizeof(T)];
    _mm_storeu_si128((__m128i *)arr, v);
    fprintf(stderr, "%-8s: [%x", hdr, arr[0] & ((1u << (8 * sizeof(T))) - 1));
    for (uint32_t i = 1; i < sizeof(__m128i) / sizeof(T); ++i) {
        fprintf(stderr, ",%x", arr[i] & ((1u << (8 * sizeof(T))) - 1));
    }
    fprintf(stderr, "]\n");
}

static const __m256i positions_v = _mm256_set_epi8(
    // clang-format off
            31, 30, 29, 28, 27, 26, 25, 24,
            23, 22, 21, 20, 19, 18, 17, 16,
            15, 14, 13, 12, 11, 10,  9,  8,
             7,  6,  5,  4,  3,  2,  1,  0
    // clang-format on
);


struct block {
    using vec_t   = __m256i;
    using index_t = std::byte;

    static constexpr uint32_t ALLOC_MASK = 0x80;

    index_t indexes[sizeof(vec_t)] __attribute__((aligned(sizeof(vec_t))));

    void
    init() {
        __builtin_memset(indexes, 31, sizeof(vec_t));
        indexes[0] = index_t(0);
    }


    uint32_t
    max_free() {
        __m128i p0 = _mm_load_si128((__m128i *)indexes);
        __m128i p1 = _mm_load_si128((__m128i *)(indexes + sizeof(__m128i)));
        PRINT_V(p0);
        PRINT_V(p1);

        __m128i m0 = _mm_max_epi8(p0, p1);
        PRINT_V(m0);

        __m128i p2 = _mm_shuffle_epi32(m0, 0x4e);
        PRINT_V(p2);
        __m128i m1 = _mm_max_epi8(m0, p2);
        PRINT_V(m1);

        __m128i p3 = _mm_shuffle_epi32(m1, 0xb1);
        PRINT_V(p3);
        __m128i m2 = _mm_max_epi8(m1, p3);
        PRINT_V(m2);

        __m128i p4 = _mm_shufflelo_epi16(m2, 0xb1);
        PRINT_V(p4);
        __m128i m3 = _mm_max_epi8(m2, p4);
        PRINT_V(m3);

        __m128i p5 = _mm_srli_si128(m3, 1);
        PRINT_V(p5);
        __m128i m4 = _mm_max_epi8(m3, p5);
        PRINT_V(m4);

        return uint32_t(_mm_extract_epi8(m4, 0));
    }

    uint32_t
    free(const uint32_t bstart) {

        const uint32_t bsize = uint32_t(indexes[bstart]) ^ ALLOC_MASK;
        assert(!(bsize & ALLOC_MASK));


        const uint32_t bsize_prev_raw = uint32_t(indexes[bstart - 1]);
        const uint32_t bsize_next_raw = uint32_t(indexes[bstart + bsize]);

        IMPOSSIBLE(bsize_prev_raw > 0xff);
        IMPOSSIBLE(bsize_next_raw > 0xff);

        const uint32_t bsize_prev =
            bsize_prev_raw & ((bsize_prev_raw - ALLOC_MASK) >> 16);
        const uint32_t bsize_next =
            bsize_next_raw & ((bsize_next_raw - ALLOC_MASK) >> 16);

        PRINT("[%x][%x]: [%x, %x] -> [%x, %x]\n", bstart, bsize, bsize_prev_raw,
              bsize_next_raw, bsize_prev, bsize_next);

        const uint32_t new_bsize  = bsize_prev + bsize + bsize_next;
        const uint32_t new_bstart = bstart - bsize_prev;

        PRINT("new_bsize: %d\nnew_bstart: %d\n", new_bsize, new_bstart);

        const vec_t bstart_v  = _mm256_set1_epi8(new_bstart);
        const vec_t bend_v    = _mm256_set1_epi8(new_bstart + new_bsize);
        const vec_t indexes_v = _mm256_load_si256((vec_t *)indexes);

        const vec_t bstart_mask_v = _mm256_cmpgt_epi8(bstart_v, positions_v);
        const vec_t bend_mask_v   = _mm256_cmpgt_epi8(bend_v, positions_v);
        PRINT_V(bstart_mask_v);
        PRINT_V(bend_mask_v);

        const vec_t bregion_mask_v =
            _mm256_xor_si256(bstart_mask_v, bend_mask_v);
        const vec_t bsize_v = _mm256_sub_epi8(bend_v, bstart_v);
        PRINT_V(bregion_mask_v);
        PRINT_V(bsize_v);

        const vec_t new_indexes_v =
            _mm256_blendv_epi8(indexes_v, bsize_v, bregion_mask_v);
        PRINT_V(new_indexes_v);
        _mm256_store_si256((vec_t *)indexes, new_indexes_v);

        return bsize;
    }

    uint32_t
    alloc(const uint32_t size) {
        const vec_t indexes_v = _mm256_load_si256((vec_t *)indexes);
        const vec_t size_v    = _mm256_set1_epi8(size);
        PRINT_V(indexes_v);
        PRINT_V(size_v);
        const vec_t matches_v = _mm256_xor_si256(
            _mm256_cmpgt_epi8(size_v, indexes_v), _mm256_set1_epi64x(-1));
        PRINT_V(matches_v);


        const uint32_t matches_mask = _mm256_movemask_epi8(matches_v);
        if (matches_mask == 0) {
            return 0;
        }

        const uint32_t match_start = _tzcnt_u32(matches_mask);


        const uint32_t match_end = match_start + size;
        const uint32_t bsize     = uint32_t(indexes[match_start]);
        const uint32_t sub_end   = match_start + bsize;

        PRINT("Match_end: %d\nSub_end: %d\n", match_end, sub_end);

        const vec_t match_end_v = _mm256_set1_epi8(match_end);
        const vec_t match_end_mask_v =
            _mm256_cmpgt_epi8(match_end_v, positions_v);
        const vec_t match_region_mask_v =
            _mm256_and_si256(match_end_mask_v, matches_v);
        PRINT_V(match_region_mask_v);

        const vec_t sub_end_v      = _mm256_set1_epi8(sub_end);
        const vec_t sub_end_mask_v = _mm256_cmpgt_epi8(sub_end_v, positions_v);
        const vec_t sub_region_mask_v =
            _mm256_xor_si256(sub_end_mask_v, match_region_mask_v);
        PRINT_V(sub_region_mask_v);


        const vec_t sub_indexes_v = _mm256_and_si256(
            _mm256_sub_epi8(indexes_v, size_v), sub_region_mask_v);
        PRINT_V(sub_indexes_v);
        const vec_t set_indexes_v =
            _mm256_and_si256(_mm256_or_si256(size_v, _mm256_set1_epi8(0x80)),
                             match_region_mask_v);

        const vec_t modified_region_v =
            _mm256_or_si256(sub_indexes_v, set_indexes_v);
        const vec_t modify_region_mask_v =
            _mm256_and_si256(sub_end_mask_v, matches_v);

        const vec_t new_indexes_v = _mm256_blendv_epi8(
            indexes_v, modified_region_v, modify_region_mask_v);

        PRINT_V(new_indexes_v);
        _mm256_store_si256((vec_t *)indexes, new_indexes_v);

        return match_start;
    }

    void
    unit_test() {
        init();

        assert(max_free() == 31);
        assert(alloc(1) == 1);
        assert(max_free() == 30);
        assert(alloc(4) == 2);
        assert(max_free() == 26);
        assert(alloc(28) == 0);
        assert(alloc(26) == 6);
        assert(max_free() == 0);
        assert(alloc(1) == 0);
        free(1);
        assert(max_free() == 1);
        assert(alloc(1) == 1);
        assert(max_free() == 0);
        free(6);
        assert(max_free() == 26);
        assert(alloc(22) == 6);
        assert(max_free() == 4);
        assert(alloc(4) == 28);
        assert(max_free() == 0);
        free(1);
        assert(max_free() == 1);
        free(2);
        assert(max_free() == 5);
        assert(alloc(5) == 1);
        assert(max_free() == 0);
        free(1);
        assert(max_free() == 5);
        free(28);
        assert(max_free() == 5);
        free(6);
        assert(max_free() == 31);
        assert(alloc(31) == 1);
        assert(max_free() == 0);
    }
};

struct block3 {
    using vec_t = __m256i;

    uint64_t indexes[sizeof(vec_t) / sizeof(uint64_t)]
        __attribute__((aligned(sizeof(vec_t))));
    uint64_t sizes[sizeof(vec_t) / sizeof(uint64_t)];

    void
    init() {
        __builtin_memset(indexes, -1, sizeof(vec_t));
        __builtin_memset(sizes, 0, sizeof(vec_t));
    }

    uint32_t
    alloc_set_bits(vec_t v, const uint32_t size) {
        const vec_t zeros = _mm256_cmpeq_epi64(v, _mm256_set1_epi64x(0));

        const uint32_t pos_mask =
            ~_mm256_movemask_pd(_mm256_castsi256_pd(zeros));
        if ((pos_mask & ((1u << sizeof(vec_t) / sizeof(uint64_t)) - 1)) == 0) {
            return 0;
        }

        const uint32_t pos = _tzcnt_u32(pos_mask);

        const uint64_t index_bv = indexes[pos];

        const uint64_t size_mask_for_indexes = (1ul << size) - 1;
        const uint64_t size_mask_for_sizes   = (1ul << (size - 1)) - 1;

        const uint32_t index_pos = _tzcnt_u64(index_bv);
        indexes[pos] ^= (size_mask_for_indexes << index_pos);
        sizes[pos] |= (size_mask_for_sizes << (index_pos + 1));

        return index_pos + 64 * pos + 1;
    }

    static vec_t
    alloc_create_vec(vec_t          indexes_v,
                     const uint32_t size,
                     const uint32_t lower_bits) {
        if (size == 2) {
            return _mm256_and_si256(_mm256_slli_epi64(indexes_v, 1), indexes_v);
        }
        vec_t lower_bits_shift = _mm256_set1_epi64x(lower_bits);
        vec_t indexes_v0       = _mm256_and_si256(
            _mm256_sllv_epi64(indexes_v, lower_bits_shift), indexes_v);
        vec_t indexes_v1 =
            _mm256_and_si256(_mm256_slli_epi64(indexes_v0, 1), indexes_v0);
        if (size < 4) {
            return indexes_v1;
        }

        vec_t indexes_v2 =
            _mm256_and_si256(_mm256_slli_epi64(indexes_v1, 2), indexes_v1);
        if (size < 8) {
            return indexes_v2;
        }

        vec_t indexes_v3 =
            _mm256_and_si256(_mm256_slli_epi64(indexes_v2, 4), indexes_v2);
        if (size < 16) {
            return indexes_v3;
        }
        return _mm256_and_si256(_mm256_slli_epi64(indexes_v3, 8), indexes_v3);
    }

    uint32_t
    alloc(const uint32_t size) {
        vec_t indexes_v = _mm256_load_si256((vec_t *)indexes);
        if (size == 1) {
            return alloc_set_bits(indexes_v, 1);
        }

        const uint32_t leading_one = 31 - _lzcnt_u32(size);
        const uint32_t lower_bits  = size & ((1u << leading_one) - 1);
        return alloc_set_bits(alloc_create_vec(indexes_v, size, lower_bits),
                              size);
    }

    void
    show() {
        fprintf(stderr,
                "--------------------------------------------------------------"
                "--------\n");
        for (uint32_t i = 0; i < 4; ++i) {
            fprintf(stderr, "%d: [%016lx, %016lx]\n", i, indexes[i], sizes[i]);
        }
        fprintf(stderr,
                "--------------------------------------------------------------"
                "--------\n");
    }
    void
    free(const uint32_t pos) {
        const uint32_t pos_x = pos / 64;
        const uint32_t pos_y = pos % 64;

        const uint64_t size_bv = sizes[pos_x];

        const uint32_t size =
            _tzcnt_u64(~(size_bv | ((1ul << pos_y) - 1))) - (pos_y - 1);

        indexes[pos_x] |= ((1ul << size) - 1) << (pos_y - 1);
        sizes[pos_x] &= ~(((1ul << size) - 1) << (pos_y - 1));
    }

    void
    unit_test() {
        init();

        assert(alloc(1) == 1);
        assert(alloc(1) == 2);
        assert(alloc(16) == 3);
        assert(alloc(8) == 19);
        free(3);
        assert(alloc(15) == 3);
        assert(alloc(1) == 18);
        assert(alloc(10) == 27);
        assert(alloc(12) == 37);
        assert(alloc(17) == 65);
        assert(alloc(16) == 49);
        free(3);
        free(18);
        assert(alloc(16) == 3);
        free(1);
        free(2);
        assert(alloc(2) == 1);
        assert(alloc(31) == 82);
        assert(alloc(31) == 129);
        assert(alloc(16) == 113);
        assert(alloc(31) == 160);
        assert(alloc(2) == 191);
        assert(alloc(9) == 193);
        free(191);
        free(193);
        assert(alloc(3) == 193);
        assert(alloc(1) == 191);
        assert(alloc(1) == 192);
        assert(alloc(31) == 196);
        assert(alloc(31) == 0);
        assert(alloc(30) == 227);
        assert(alloc(1) == 0);
    }
};

struct block2 {
    uint32_t idx;
    uint32_t sidx;
    void
    init() {
        idx  = ~1;
        sidx = ~1;
    }


    void
    show() {
        fprintf(stderr, "%-8s: %08x\n", "idx", idx);
        fprintf(stderr, "%-8s: %08x\n", "sidx", sidx);
    }
    uint32_t
    alloc(const uint32_t size) {
        if (idx == 0) {
            return 0;
        }
        if (size == 1) {
            const uint32_t _idx = idx;
            const uint32_t ret  = _tzcnt_u32(_idx);
            idx                 = (_idx & (_idx - 1));
            return ret;
        }
        const uint32_t leading_one = 31 - _lzcnt_u32(size);
        const uint32_t lower_bits  = _bzhi_u32(size, leading_one);
        const uint32_t shifted_idx = alloc_position_vec(size, lower_bits);
        if (shifted_idx == 0) {
            return 0;
        }
        const uint32_t ret_idx = _tzcnt_u32(shifted_idx);
        idx ^= ((1u << size) - 1) << ret_idx;
        sidx &= ~(((1u << (size - 1)) - 1) << (ret_idx + 1));
        return ret_idx;
    }

    uint32_t
    alloc_position_vec(const uint32_t size, const uint32_t lower_bits) {
        uint32_t _idx = idx;
        if (size == 2) {
            return _idx & (_idx >> 1);
        }
        _idx &= (_idx >> lower_bits);
        _idx &= (_idx >> 1);
        if (size < 4) {
            return _idx;
        }
        _idx &= (_idx >> 2);
        if (size < 8) {
            return _idx;
        }
        _idx &= (_idx >> 4);
        if (size < 16) {
            return _idx;
        }
        return _idx & (_idx >> 8);
    }

    void
    free(const uint32_t pos) {
        const uint64_t _idx  = idx;
        const uint32_t start = ((1ul << 32) | _idx) >> pos;
        const uint32_t size  = _tzcnt_u32(start);
        idx |= ((1u << size) - 1) << pos;
        sidx |= ((1u << size) - 1) << pos;
    }
};

using test_t = block3;
#define TEST_N 10000
void
test() {
    test_t                    b;
    timer<timer_conf::CYCLES> t;
    b.init();
    b.unit_test();
    b.init();

    uint32_t   ret_idx = 0;
    uint32_t   rets[32];
    uint32_t * sizes = (uint32_t *)calloc(TEST_N, sizeof(uint32_t));

    for (uint32_t i = 0; i < TEST_N; ++i) {
        while (sizes[i] == 0) {
            sizes[i] = rand() % 31;
        }
    }

    t.start();
    COMPILER_BARRIER();
    for (uint32_t i = 0; i < TEST_N; ++i) {
        uint32_t r = b.alloc(sizes[i]);
        COMPILER_DO_NOT_OPTIMIZE_OUT(r);
        if (!r) {

            for (uint32_t j = 0; j < ret_idx; ++j) {
                b.free(rets[j]);
            }
            ret_idx = 0;
        }
        else {
            rets[ret_idx] = r;
            ++ret_idx;
        }
    }
    COMPILER_BARRIER();
    t.end();
    t.std_print(TEST_N);
}
int
main(int argc, char ** argv) {
    test();
}
