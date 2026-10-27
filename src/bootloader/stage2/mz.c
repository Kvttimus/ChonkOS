#include "mz.h"
#include "memory.h"
#include "stdio.h"

#define MZ_SIGNATURE    0x5A4D      // "MZ"

#pragma pack(push, 1)

typedef struct
{
    uint16_t Signature;
    uint16_t BytesInLastPage;       // 0 means the last page is full
    uint16_t Pages;                 // file size in 512 byte pages, including this header
    uint16_t RelocationCount;
    uint16_t HeaderParagraphs;      // header size in 16 byte paragraphs, the image starts after it
    uint16_t MinExtraParagraphs;    // memory needed after the image (BSS + stack)
    uint16_t MaxExtraParagraphs;
    uint16_t InitialSS;             // relative to the start of the image
    uint16_t InitialSP;
    uint16_t Checksum;
    uint16_t InitialIP;
    uint16_t InitialCS;             // relative to the start of the image
    uint16_t RelocationTableOffset;
    uint16_t OverlayNumber;
} MZ_Header;

// a spot in the image holding a segment value that needs the image's real segment added to it
typedef struct
{
    uint16_t Offset;
    uint16_t Segment;
} MZ_Relocation;

#pragma pack(pop)

bool MZ_Prepare(uint16_t loadSegment, uint32_t fileSize, uint32_t memoryAvailable, MZ_StartInfo* startOut)
{
    MZ_Header __far* header = MK_FP(loadSegment, 0);
    MZ_Relocation __far* relocations;
    uint32_t headerSize, imageSize, extraSize;
    uint16_t imageSegment;
    uint16_t i;
    uint8_t __far* bss;

    if (fileSize < sizeof(MZ_Header) || header->Signature != MZ_SIGNATURE)
    {
        puts("MZ: not an MZ executable\n");
        return false;
    }

    headerSize = (uint32_t)header->HeaderParagraphs * 16;
    imageSize = (uint32_t)header->Pages * 512;
    if (header->BytesInLastPage != 0)
    {
        imageSize -= 512 - header->BytesInLastPage;
    }
    imageSize -= headerSize;
    extraSize = (uint32_t)header->MinExtraParagraphs * 16;

    if (headerSize + imageSize > fileSize
        || (uint32_t)header->RelocationTableOffset + (uint32_t)header->RelocationCount * sizeof(MZ_Relocation) > headerSize)
    {
        puts("MZ: corrupt header\n");
        return false;
    }
    if (headerSize + imageSize + extraSize > memoryAvailable)
    {
        puts("MZ: not enough memory\n");
        return false;
    }

    // the image runs right where it was loaded, just past the header
    imageSegment = loadSegment + header->HeaderParagraphs;

    // the linker assumed the image would be at segment 0, so every segment value it wrote
    // (far calls, DGROUP, ...) needs the real segment added to it
    relocations = MK_FP(loadSegment, header->RelocationTableOffset);
    for (i = 0; i < header->RelocationCount; i++)
    {
        uint16_t __far* target = MK_FP(imageSegment + relocations[i].Segment, relocations[i].Offset);
        *target += imageSegment;
    }

    // BSS isn't stored in the file, zero it 1 paragraph at a time so we never wrap past 64KiB
    bss = MK_FP(imageSegment + (uint16_t)(imageSize / 16), (uint16_t)(imageSize % 16));
    for (i = 0; i < header->MinExtraParagraphs; i++)
    {
        uint8_t j;
        for (j = 0; j < 16; j++)
        {
            bss[j] = 0;
        }
        bss = MK_FP(FP_SEG(bss) + 1, FP_OFF(bss));
    }

    startOut->cs = imageSegment + header->InitialCS;
    startOut->ip = header->InitialIP;
    startOut->ss = imageSegment + header->InitialSS;
    startOut->sp = header->InitialSP;
    return true;
}
