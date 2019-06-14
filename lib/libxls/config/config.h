/* Define to 1 if you have the `asprintf' function. */
#define HAVE_ASPRINTF 1

/* Define if you have the iconv() function. */
#if (!defined(__MINGW32__) && !defined(mingw32_HOST_OS)) || defined (FORCE_HAS_ICONV)
#define HAVE_ICONV 1
#define ICONV_CONST
#endif

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.5.1"
