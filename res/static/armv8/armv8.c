/*
    ARM v8 Hardware Accelerated Code for CRC32 CRC32C and AES Processing in mORMot 2.
    Since the FPC AARCH64 inlined asm is very limited, we used C code and static linking.
    
*/

#include <stdint.h>
#include <arm_neon.h>

#define CRC32X(crc, value) __asm__("crc32x %w[c], %w[c], %x[v]":[c]"+r"(crc):[v]"r"(value))
#define CRC32W(crc, value) __asm__("crc32w %w[c], %w[c], %w[v]":[c]"+r"(crc):[v]"r"(value))
#define CRC32H(crc, value) __asm__("crc32h %w[c], %w[c], %w[v]":[c]"+r"(crc):[v]"r"(value))
#define CRC32B(crc, value) __asm__("crc32b %w[c], %w[c], %w[v]":[c]"+r"(crc):[v]"r"(value))
#define CRC32CX(crc, value) __asm__("crc32cx %w[c], %w[c], %x[v]":[c]"+r"(crc):[v]"r"(value))
#define CRC32CW(crc, value) __asm__("crc32cw %w[c], %w[c], %w[v]":[c]"+r"(crc):[v]"r"(value))
#define CRC32CH(crc, value) __asm__("crc32ch %w[c], %w[c], %w[v]":[c]"+r"(crc):[v]"r"(value))
#define CRC32CB(crc, value) __asm__("crc32cb %w[c], %w[c], %w[v]":[c]"+r"(crc):[v]"r"(value))

uint32_t crc32arm64(uint32_t crc, const uint8_t *p, unsigned int len)
{
	int64_t length = len;
  crc = ~crc;
  
  if (length >= 32)
  {
    // align reads to 8 bytes
    if ((uint64_t)p & sizeof(uint32_t)) {
      CRC32W(crc, *((uint32_t *)p));
      p += sizeof(uint32_t);
      length -= sizeof(uint32_t);
    }
    if ((uint64_t)p & sizeof(uint16_t)) {
      CRC32H(crc, *((uint16_t *)p));
      p += sizeof(uint16_t);
      length -= sizeof(uint16_t);
    }
    if ((uint64_t)p & sizeof(uint8_t))
    {
      CRC32B(crc, *p);
      p++;
      length--;
    }
    // handle 32 bytes per loop
    while ((length -= 32) >= 0) {
      CRC32X(crc, *((uint64_t *)p));
      p += sizeof(uint64_t);
      CRC32X(crc, *((uint64_t *)p));
      p += sizeof(uint64_t);
      CRC32X(crc, *((uint64_t *)p));
      p += sizeof(uint64_t);
      CRC32X(crc, *((uint64_t *)p));
      p += sizeof(uint64_t);
    }
  }
  // remaining 0..31 bytes
	if (length & 16) {
		CRC32X(crc, *((uint64_t *)p));
		p += sizeof(uint64_t);
		CRC32X(crc, *((uint64_t *)p));
		p += sizeof(uint64_t);
	}
	if (length & sizeof(uint64_t)) {
		CRC32X(crc, *((uint64_t *)p));
		p += sizeof(uint64_t);
	}
	if (length & sizeof(uint32_t)) {
		CRC32W(crc, *((uint32_t *)p));
		p += sizeof(uint32_t);
	}
	if (length & sizeof(uint16_t)) {
		CRC32H(crc, *((uint16_t *)p));
		p += sizeof(uint16_t);
	}
	if (length & sizeof(uint8_t))
		CRC32B(crc, *p);

	return ~crc;
}

uint32_t crc32carm64(uint32_t crc, const uint8_t *p, unsigned int len)
{
	int64_t length = len;
  crc = ~crc;

  if (length >= 32)
  {
    // align reads to 8 bytes
    if ((uint64_t)p & sizeof(uint32_t)) {
      CRC32CW(crc, *((uint32_t *)p));
      p += sizeof(uint32_t);
      length -= sizeof(uint32_t);
    }
    if ((uint64_t)p & sizeof(uint16_t)) {
      CRC32CH(crc, *((uint16_t *)p));
      p += sizeof(uint16_t);
      length -= sizeof(uint16_t);
    }
    if ((uint64_t)p & sizeof(uint8_t))
    {
      CRC32CB(crc, *p);
      p++;
      length--;
    }
    // handle 32 bytes per loop
    while ((length -= 32) >= 0) {
      CRC32CX(crc, *((uint64_t *)p));
      p += sizeof(uint64_t);
      CRC32CX(crc, *((uint64_t *)p));
      p += sizeof(uint64_t);
      CRC32CX(crc, *((uint64_t *)p));
      p += sizeof(uint64_t);
      CRC32CX(crc, *((uint64_t *)p));
      p += sizeof(uint64_t);
    }
  }
  // remaining 0..31 bytes
	if (length & 16) {
		CRC32CX(crc, *((uint64_t *)p));
		p += sizeof(uint64_t);
		CRC32CX(crc, *((uint64_t *)p));
		p += sizeof(uint64_t);
	}
	if (length & sizeof(uint64_t)) {
		CRC32CX(crc, *((uint64_t *)p));
		p += sizeof(uint64_t);
	}
	if (length & sizeof(uint32_t)) {
		CRC32CW(crc, *((uint32_t *)p));
		p += sizeof(uint32_t);
	}
	if (length & sizeof(uint16_t)) {
		CRC32CH(crc, *((uint16_t *)p));
		p += sizeof(uint16_t);
	}
	if (length & sizeof(uint8_t))
		CRC32CB(crc, *p);

	return ~crc;
}

uint32_t crc32cby4arm64(uint32_t crc, uint32_t value)
{
		CRC32CW(crc, value);
    return crc;
}

void crc32blockarm64(uint32_t *crc128, uint32_t *data128)
{
    uint32_t crc0 = crc128[0];
    uint32_t crc1 = crc128[1];
    uint32_t crc2 = crc128[2];
    uint32_t crc3 = crc128[3];
		CRC32CW(crc0, data128[0]);
		CRC32CW(crc1, data128[1]);
		CRC32CW(crc2, data128[2]);
		CRC32CW(crc3, data128[3]);
    crc128[0] = crc0;
    crc128[1] = crc1;
    crc128[2] = crc2;
    crc128[3] = crc3;
}

void crc32blocksarm64(uint32_t *crc128, uint32_t *data128, int count)
{
    uint32_t crc0 = crc128[0];
    uint32_t crc1 = crc128[1];
    uint32_t crc2 = crc128[2];
    uint32_t crc3 = crc128[3];
    while (count > 0)
    {
  		CRC32CW(crc0, data128[0]);
	  	CRC32CW(crc1, data128[1]);
		  CRC32CW(crc2, data128[2]);
		  CRC32CW(crc3, data128[3]);
      data128 += 4;
      count--;
    }
    crc128[0] = crc0;
    crc128[1] = crc1;
    crc128[2] = crc2;
    crc128[3] = crc3;
}

static void aesencrypt(uint8_t* keys, uint32_t rounds,
                     const uint8_t* inBlock, uint8_t* outBlock)
{
  uint8x16_t block = vld1q_u8(inBlock);

  while (rounds > 1) // will be properly inlined in 128/192/256 functions below
  {
    block = vaesmcq_u8(vaeseq_u8(block, vld1q_u8(keys)));
    keys += 16;
    rounds--;
  }
  vst1q_u8(outBlock, veorq_u8(vaeseq_u8(block, vld1q_u8(keys)), vld1q_u8(keys+16)));
}

void aesencryptarm128(uint8_t* keys, const uint8_t* inBlock, uint8_t* outBlock)
{
  aesencrypt(keys, 10, inBlock, outBlock);
}

void aesencryptarm192(uint8_t* keys, const uint8_t* inBlock, uint8_t* outBlock)
{
  aesencrypt(keys, 12, inBlock, outBlock);
}

void aesencryptarm256(uint8_t* keys, const uint8_t* inBlock, uint8_t* outBlock)
{
  aesencrypt(keys, 14, inBlock, outBlock);
}

// this function doesn't give the proper results -> use plain MakeDecrKeyPas
void MakeDecrKeyArm(uint32_t rounds, uint8_t* keys)
{
  uint8x16_t block;
    
  while (rounds > 0)
  {
    keys += 16;
    vst1q_u8(keys, vaesimcq_u8(vld1q_u8(keys)));
    rounds--;
  }
}

static void aesdecrypt(uint8_t* keys, uint32_t rounds,
                     const uint8_t* inBlock, uint8_t* outBlock)
{
  uint8x16_t block = vld1q_u8(inBlock);
  keys += 16 * rounds;

  while (rounds > 1) // will be properly inlined in 128/192/256 functions below
  {
    block = vaesimcq_u8(vaesdq_u8(block, vld1q_u8(keys)));
    keys -= 16;
    rounds--;
  }
  vst1q_u8(outBlock, veorq_u8(vaesdq_u8(block, vld1q_u8(keys)), vld1q_u8(keys-16)));
}

void aesdecryptarm128(uint8_t* keys, const uint8_t* inBlock, uint8_t* outBlock)
{
  aesdecrypt(keys, 10, inBlock, outBlock);
}

void aesdecryptarm192(uint8_t* keys, const uint8_t* inBlock, uint8_t* outBlock)
{
  aesdecrypt(keys, 12, inBlock, outBlock);
}

void aesdecryptarm256(uint8_t* keys, const uint8_t* inBlock, uint8_t* outBlock)
{
  aesdecrypt(keys, 14, inBlock, outBlock);
}

// the vmull_p64 intrinsic uses the wrong argument types
#define vmull_low_p64(A, B) ({                               \
	poly128_t res__;                                         \
	asm("pmull    %0.1q, %1.1d, %2.1d                \n\t"   \
        : "=w" (res__) : "w" (A), "w" (B) );                 \
    res__;                                                   \
})

// GCM multiplication: a = a times b in GF(2^128)
void gf_mul_h_arm(unsigned char* a, const unsigned char* b )
{
	/* vector variables */
	uint8x16_t a_p, b_p; /* inputs */
	uint8x16_t z, p; /* constants */
	uint8x16_t r0, r1; /* full width multiply result (before reduction) */
	uint8x16_t t0, t1; /* temps */
	uint8x16_t c_p; /* output */

	/* reverse bits in each byte to convert from gcm format to little-little endian */
	a_p = vrbitq_u8( vld1q_u8( a ) );
	b_p = vrbitq_u8( vld1q_u8( b ) );

	/* polynomial multiply (128*128->256bit). See [GCM-WP] algorithms 3. */
	z = vdupq_n_u8( 0 );
	r0 = (uint8x16_t)vmull_low_p64( (poly64x2_t)a_p, (poly64x2_t)b_p );
	r1 = (uint8x16_t)vmull_high_p64( (poly64x2_t)a_p, (poly64x2_t)b_p );
	t0 = vextq_u8( b_p, b_p, 8 );
	t1 = (uint8x16_t)vmull_low_p64( (poly64x2_t)a_p, (poly64x2_t)t0 );
	t0 = (uint8x16_t)vmull_high_p64( (poly64x2_t)a_p, (poly64x2_t)t0 );
	t0 = veorq_u8( t0, t1 );
	t1 = vextq_u8( z, t0, 8 );
	r0 = veorq_u8( r0, t1 );
	t1 = vextq_u8( t0, z, 8 );
	r1 = veorq_u8( r1, t1 );

	/* polynomial reduction (256->128bit). See [GCM-WP] algorithms 5. */
	p = (uint8x16_t)vdupq_n_u64( 0x0000000000000087 );
	t0 = (uint8x16_t)vmull_high_p64( (poly64x2_t)r1, (poly64x2_t)p );
	t1 = vextq_u8( t0, z, 8 );
	r1 = veorq_u8( r1, t1 );
	t1 = vextq_u8( z, t0, 8 );
	r0 = veorq_u8( r0, t1 );
	t0 = (uint8x16_t)vmull_low_p64( (poly64x2_t)r1, (poly64x2_t)p );
	c_p = veorq_u8( r0, t0 );

	/* reverse bits in each byte to convert from little-little endian to gcm format */
	vst1q_u8( a, vrbitq_u8( c_p ) );
}
