#include "libccap.h"

#include <cstdio>
#include <cstring>
#include <string_view>
#include <vector>
#include <charconv>
#include <cassert>
#include <stdexcept>
#include <fstream>

#include <openssl/aes.h>
#include <openssl/evp.h>

void ccap_aes_encrypt_128_func(const CCapU128* secret, const CCapU128* data, CCapU128* result) {
    EVP_CIPHER_CTX *e_ctx = EVP_CIPHER_CTX_new();
    EVP_CIPHER_CTX_init(e_ctx);

    // only one block at a time, IV irrelevant
    uint8_t iv[32] = {0};

    // This saves the key expansion in other memory, so if (result) == (secret) we can overwrite (result) without breaking the cipher
    EVP_EncryptInit_ex(e_ctx, EVP_aes_128_cbc(), NULL, *secret, iv);

    int saved_len = 0;
    EVP_EncryptUpdate(e_ctx, *result, &saved_len, *data, 16);

    EVP_EncryptFinal_ex(e_ctx, NULL, NULL);

    EVP_CIPHER_CTX_free(e_ctx);
}

uint64_t ccap_panic_write_utf8(const uint8_t *utf8, uint64_t utf_len) {
    fwrite(utf8, sizeof(uint8_t), utf_len, stderr);
    return utf_len;
}

void ccap_panic_complete() {
    exit(EXIT_FAILURE);
}

void u128_from_hex_str(std::string_view str, CCapU128& u128) {
    // the string is a big-endian hex string
    assert(str.length() == 32);
    const char* c = str.begin();
    std::from_chars(c +  0, c +  2, u128[15], 16);
    std::from_chars(c +  2, c +  4, u128[14], 16);
    std::from_chars(c +  4, c +  6, u128[13], 16);
    std::from_chars(c +  6, c +  8, u128[12], 16);
    std::from_chars(c +  8, c + 10, u128[11], 16);
    std::from_chars(c + 10, c + 12, u128[10], 16);
    std::from_chars(c + 12, c + 14, u128[ 9], 16);
    std::from_chars(c + 14, c + 16, u128[ 8], 16);
    std::from_chars(c + 16, c + 18, u128[ 7], 16);
    std::from_chars(c + 18, c + 20, u128[ 6], 16);
    std::from_chars(c + 20, c + 22, u128[ 5], 16);
    std::from_chars(c + 22, c + 24, u128[ 4], 16);
    std::from_chars(c + 24, c + 26, u128[ 3], 16);
    std::from_chars(c + 26, c + 28, u128[ 2], 16);
    std::from_chars(c + 28, c + 30, u128[ 1], 16);
    std::from_chars(c + 30, c + 32, u128[ 0], 16);
}

void u64_from_hex_str(std::string_view str, uint64_t& u64) {
    // the string is a big-endian hex string
    assert(str.length() == 16);
    const char* c = str.begin();
    std::from_chars(c + 0, c + 16, u64, 16);
}

struct Test {
    CCapU128 secret;
    CCap2024_11 cap;
    uint64_t base;
    bool top_64;
    uint64_t top;
    CCapPerms expected_perms;
    CCapResult expected_result;
};

Test test_case_from_line(std::string_view str) {
    Test test{};

    std::vector<std::string_view> columns{};
    // https://stackoverflow.com/a/14266139
    auto pos = 0;
    while ((pos = str.find(":")) != std::string::npos) {
        auto col = str.substr(0, pos);
        // fprintf(stdout, "Col %ld: %.*s\n", columns.size(), (int)col.length(), col.begin());
        columns.push_back(col);
        str.remove_prefix(pos + 1);
    }
    if (!str.empty()) {
        // fprintf(stdout, "Col %ld: %.*s\n", columns.size(), (int)str.length(), str.begin());
        columns.push_back(str);
    }
    assert(columns.size() == 7);

    u128_from_hex_str(columns[0], test.secret);
    u128_from_hex_str(columns[1], test.cap.signature);
    u128_from_hex_str(columns[2], test.cap.data);
    u64_from_hex_str(columns[3], test.base);
    assert(columns[4].substr(0, 15).compare("000000000000000") == 0);
    switch (columns[4][15]) {
        case '0':
            test.top_64 = false;
            break;
        case '1':
            test.top_64 = true;
            break;
        default:
            throw std::runtime_error("Invalid CSV top");
    }
    u64_from_hex_str(columns[4].substr(16,16), test.top);

    switch (columns[5][0]) {
        case 'r':
            test.expected_perms = CCapPerms_Read;
            break;
        case 'w':
            test.expected_perms = CCapPerms_Write;
            break;
        case 'b':
            test.expected_perms = CCapPerms_ReadWrite;
            break;
        default:
            throw std::runtime_error("Invalid CSV perms column");
    }

    switch (columns[6][0]) {
        case '0':
            test.expected_result = CCapResult_Success;
            break;
        case '1':
            test.expected_result = CCapResult_Decode_InvalidCapPermsChain;
            break;
        case '2':
            test.expected_result = CCapResult_Decode_InvalidCaveat;
            break;
        case '3':
            test.expected_result = CCapResult_Decode_InvalidSignature;
            break;
        case '4':
            test.expected_result = CCapResult_Decode_UnexpectedCaveat;
            break;
        default:
            throw std::runtime_error("Invalid CSV results column");
    }

    // Cap2024_11_Bits bits = Cap2024_11_Bits_unpack(&test.cap.data);
    // fprintf(stdout, "Cap { enc_elem_width: %d, b_c: 0x%x, index: %d, index_size_div: %d, range_x: %d, range_y_minus_one: %d }\n", bits.encoded_elem_width, bits.b_c, bits.index, bits.index_size_div, bits.range_x, bits.range_y_minus_one);

    return test;
}

// return true if x encompasses y
bool encompasses(uint64_t x_base, uint64_t x_len, bool x_len_64, uint64_t y_base, uint64_t y_len, bool y_len_64) {
    if (x_base > y_base) {
        return false;
    }

    uint64_t x_top = x_base + x_len;
    bool x_top_64 = (x_top < x_base) || x_len_64;

    uint64_t y_top = y_base + y_len;
    bool y_top_64 = (y_top < y_base) || y_len_64;

    // if x_top < y_top return false
    if (
        ((x_top_64 == y_top_64) && (x_top < y_top))
        || y_top_64 // x_top_64 is false => {x_top_64, x_top} < {y_top_64, y_top}
    ) {
        return false;
    }

    return true;
}

int main(int argc, char **argv) {
    if (argc != 2 || strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0) {
        fprintf(stderr, "Usage: %s <CSV file of test cases>\n", argv[0]);
        return EXIT_FAILURE;
    }

    // https://stackoverflow.com/questions/7868936/read-file-line-by-line-using-ifstream-in-c
    std::ifstream infile(argv[1]);
    std::string line;
    // ignore first line
    std::getline(infile, line);
    uint64_t line_no = 2;
    while (std::getline(infile, line)) {
        Test test = test_case_from_line(line);

        CCapPerms perms;
        uint64_t base;
        uint64_t len;
        bool len_64;

        CCapResult perms_res = ccap2024_11_read_perms(&test.cap, &perms);
        CCapResult range_res = ccap2024_11_read_range(&test.cap, &base, &len, &len_64);
        CCapResult sign_res = ccap2024_11_check_signature(&test.cap, &test.secret);

        // Test if we can roundtrip a virtio descriptor
        if (range_res == CCapResult_Success && !len_64 && (len <= UINT32_MAX) && (len > 0) && (perms != CCapPerms_ReadWrite)) {
            CCapNativeVirtqDesc desc;
            CCapResult read_virtio_res = ccap2024_11_read_virtio(&test.cap, &desc);
            if (read_virtio_res == CCapResult_Success) {
                if (desc.addr != base || desc.len != len) {
                    fprintf(stderr, "Virtio desc for line %ld not equal to the original\nvirtio base 0x%lx len 0x%016x\n  exact base 0x%lx len 0x%d%016lx\n", line_no, desc.addr, desc.len, base, len_64, len);
                }

                uint16_t extracted_flags = ccap2024_11_read_virtio_flags_indirect_next(&test.cap);
                if (extracted_flags != (desc.flags & ~CCAP_VIRTQ_F_WRITE)) {
                    fprintf(stderr, "Virtio desc for line %ld has extracted flags that don't match directly extracted flags:\nfrom read_virtio %x\nfrom read_virtio_flags_indirect_next %x\n", line_no, (desc.flags & ~CCAP_VIRTQ_F_WRITE), extracted_flags);
                }

                uint16_t extracted_next = ccap2024_11_read_virtio_next(&test.cap);
                if (extracted_next != desc.next) {
                    fprintf(stderr, "Virtio desc for line %ld has extracted next that don't match directly extracted next:\nfrom read_virtio      %x\nfrom read_virtio_next %x\n", line_no, desc.next, extracted_next);
                }

                {
                    CCap2024_11 zeroed_with_next;
                    assert(ccap2024_11_clear_and_write_virtio_next(&zeroed_with_next, extracted_next) == CCapResult_Success);

                    uint16_t new_extracted_next = ccap2024_11_read_virtio_next(&test.cap);
                    if (extracted_next != new_extracted_next) {
                        fprintf(stderr, "Virtio desc for line %ld clear_and_write_next didn't put in the right next:\nexpected %x\nafter ccap2024_11_clear_and_write_virtio_next %x\n", line_no, extracted_next, new_extracted_next);
                    }
                }

                uint32_t full_secret_id;
                CCapResult read_secret_id_res = ccap2024_11_read_secret_id(&test.cap, &full_secret_id);
                if (read_secret_id_res == CCapResult_Success) {
                    CCap2024_11 reencoded_cap;
                    CCapResult init_virtio_cavs_exact_res = ccap2024_11_init_virtio_cavs_exact(
                        &reencoded_cap,
                        &test.secret,
                        &desc,
                        full_secret_id & 0xFF
                    );
                    if (init_virtio_cavs_exact_res == CCapResult_Success) {
                        uint64_t reencoded_base, reencoded_len;
                        bool reencoded_len_64;
                        assert(ccap2024_11_read_range(&reencoded_cap, &reencoded_base, &reencoded_len, &reencoded_len_64) == CCapResult_Success);

                        if (reencoded_base != base || reencoded_len != len || reencoded_len_64 != len_64) {
                            fprintf(stderr, "Reencoding line %ld via virtio produced a capability not equal to the original\nvirtio base 0x%lx len 0x%d%016lx\n exact base 0x%lx len 0x%d%016lx\n", line_no, reencoded_base, reencoded_len_64, reencoded_len, base, len_64, len);
                        }

                        uint32_t reencoded_secret_id;
                        assert(ccap2024_11_read_secret_id(&reencoded_cap, &reencoded_secret_id) == CCapResult_Success);
                        if (reencoded_secret_id != full_secret_id) {
                            fprintf(stderr, "Reencoding line %ld via virtio produced different flags\noriginal secret id = %x new = %x\n", line_no, full_secret_id, reencoded_secret_id);
                        }
                    } else {
                        fprintf(stderr, "Failed to reencode cap from virtio on line %ld %s\n", line_no, ccap_result_str(init_virtio_cavs_exact_res));
                    }
                } else {
                    fprintf(stderr, "Failed to read full secret_key_id from cap on line %ld %s\n", line_no, ccap_result_str(read_secret_id_res));
                }
            } else {
                fprintf(stderr, "Reading virtio for line %ld with ccap2024_11_read_virtio() produced a failure %s\n", line_no, ccap_result_str(read_virtio_res));
            }

            
            
        }

        // Test that we can init_inexact and init_cavs_exact for these encoded bounds and get back valid capabilities
        if (range_res == CCapResult_Success && !len_64 && len > 0) {
            CCap2024_11 reencoded_cap;
            CCapResult init_inexact_res = ccap2024_11_init_inexact(
                &reencoded_cap, &test.secret, base, len, 0, CCapPerms_ReadWrite
            );
            if (init_inexact_res == CCapResult_Success) {
                uint64_t inexact_base, inexact_len;
                bool inexact_len_64;
                assert(ccap2024_11_read_range(&reencoded_cap, &inexact_base, &inexact_len, &inexact_len_64) == CCapResult_Success);

                if (!encompasses(
                    inexact_base, inexact_len, inexact_len_64,
                    base, len, len_64
                )) {
                    fprintf(stderr, "Reencoding line %ld with ccap2024_11_init_inexact() produced a capability not encompassing the original\ninexact base 0x%lx len 0x%d%016lx\n  exact base 0x%lx len 0x%d%016lx\n", line_no, inexact_base, inexact_len_64, inexact_len, base, len_64, len);
                }
            } else {
                fprintf(stderr, "Reencoding line %ld with ccap2024_11_init_inexact() produced an error %s\n", line_no, ccap_result_str(init_inexact_res));
            }
            
            CCapResult init_exact_cav_res = ccap2024_11_init_cavs_exact(
                &reencoded_cap, &test.secret, base, len, 0, CCapPerms_ReadWrite
            );
            if (init_exact_cav_res == CCapResult_Success) {
                uint64_t exact_base, exact_len;
                bool exact_len_64;
                assert(ccap2024_11_read_range(&reencoded_cap, &exact_base, &exact_len, &exact_len_64) == CCapResult_Success);

                if (base != exact_base || len != exact_len || len_64 != exact_len_64) {
                    fprintf(stderr, "Reencoding line %ld with ccap2024_11_init_cavs_exact() produced a capability not exactly equal to the original\n", line_no);
                }
            } else {
                fprintf(stderr, "Reencoding line %ld with ccap2024_11_init_cavs_exact() produced an error %s\n", line_no, ccap_result_str(init_exact_cav_res));
            }
        }

        uint64_t top = base + len;
        bool top_64 = (top < base) || len_64;

        switch (test.expected_result) {
            case CCapResult_Success:
                if (perms_res == CCapResult_Success) {
                    if (perms != test.expected_perms) {
                        fprintf(stderr, "Expected line %ld to decode perms %s, but it decoded %s\n", line_no, ccap_perms_str(test.expected_perms), ccap_perms_str(perms));
                    }
                } else {
                    fprintf(stderr, "Expected line %ld to decode perms, but it failed with %s\n", line_no, ccap_result_str(perms_res));
                }
                
                if (range_res == CCapResult_Success) {
                    if (base != test.base || top != test.top || top_64 != test.top_64) {
                        fprintf(stderr, "Expected line %ld to have base 0x%lx top 0x%d%016lx, got base 0x%lx top 0x%d%016lx\n", line_no, test.base, test.top_64, test.top, base, top_64, top);
                    }
                } else {
                    fprintf(stderr, "Expected line %ld to success decoding range, but it failed with %s\n", line_no, ccap_result_str(range_res));
                }

                if (sign_res != CCapResult_Success) {
                    fprintf(stderr, "Expected line %ld to have a correct signature, but sigcheck failed with %s\n", line_no, ccap_result_str(sign_res));
                }
                break;
            case CCapResult_Decode_InvalidCapPermsChain:
                if (perms_res != CCapResult_Decode_InvalidCapPermsChain)
                    fprintf(stderr, "Expected line %ld to fail decoding perms, but it didn't\n", line_no);
                if (range_res == CCapResult_Success) {
                    if (base != test.base || top != test.top || top_64 != test.top_64) {
                        fprintf(stderr, "Expected line %ld to have base 0x%lx top 0x%d%016lx, got base 0x%lx top 0x%d%016lx\n", line_no, test.base, test.top_64, test.top, base, top_64, top);
                    }
                }
                break;
            case CCapResult_Decode_InvalidSignature:
                if (perms_res == CCapResult_Success) {
                    if (perms != test.expected_perms) {
                        fprintf(stderr, "Expected line %ld to decode perms %s, but it decoded %s\n", line_no, ccap_perms_str(test.expected_perms), ccap_perms_str(perms));
                    }
                } else {
                    fprintf(stderr, "Expected line %ld to decode perms, but it failed with %s\n", line_no, ccap_result_str(perms_res));
                }
                
                if (range_res == CCapResult_Success) {
                    if (base != test.base || top != test.top || top_64 != test.top_64) {
                        fprintf(stderr, "Expected line %ld to have base 0x%lx top 0x%d%016lx, got base 0x%lx top 0x%d%016lx\n", line_no, test.base, test.top_64, test.top, base, top_64, top);
                    }
                } else {
                    fprintf(stderr, "Expected line %ld to success decoding range, but it failed with %s\n", line_no, ccap_result_str(range_res));
                }

                if (sign_res != CCapResult_Decode_InvalidSignature) {
                    fprintf(stderr, "Expected line %ld to have an incorrect signature, but it returned %s instead\n", line_no, ccap_result_str(sign_res));
                }
            default:
                if (perms_res == CCapResult_Success) {
                    if (perms != test.expected_perms) {
                        fprintf(stderr, "Expected line %ld to decode perms %s, but it decoded %s\n", line_no, ccap_perms_str(test.expected_perms), ccap_perms_str(perms));
                    }
                } else {
                    fprintf(stderr, "Expected line %ld to decode perms, but it failed with %s\n", line_no, ccap_result_str(perms_res));
                }

                if (range_res != test.expected_result) {
                    fprintf(stderr, "Expected line %ld to fail decode with error %s, got %s instead\n", line_no, ccap_result_str(test.expected_result), ccap_result_str(range_res));
                }
                break;
        }

        line_no++;
    }
    

    return EXIT_SUCCESS;
}