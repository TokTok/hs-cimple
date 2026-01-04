{
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Language.Cimple.Lexer
    ( Alex
    , AlexPosn (..)
    , alexError
    , alexScanTokens
    , alexMonadScan
    , Lexeme (..)
    , lexemeClass
    , lexemePosn
    , lexemeText
    , lexemeLine
    , runAlex
    , pushContext
    , popContext
    , getContext
    , Context (..)
    , ParseError (..)
    ) where

import           Data.Aeson             (FromJSON, FromJSONKey (..),
                                         FromJSONKeyFunction (..), ToJSON,
                                         ToJSONKey (..), ToJSONKeyFunction (..),
                                         toEncoding, toJSON)
import qualified Data.Aeson.Types       as Aeson
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import           Data.Hashable          (Hashable (..))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           GHC.Generics           (Generic)
import           Language.Cimple.Ast    (Node)
import           Language.Cimple.Tokens (LexemeClass (..))
import           Test.QuickCheck        (Arbitrary (..))
}

%wrapper "monadUserState-bytestring"

tokens :-

-- Ignore attributes.
<0>		"GNU_PRINTF"				{ mkL KwGnuPrintf }
<0>		"VLA"					{ mkL KwVla }

-- Winapi functions.
<0>		"FormatMessageA"			{ mkL IdVar }
<0>		"GetAdaptersInfo"			{ mkL IdVar }
<0>		"GetSystemTimeAsFileTime"		{ mkL IdVar }
<0>		"GetTickCount"				{ mkL IdVar }
<0>		"LocalFree"				{ mkL IdVar }
<0>		"QueryPerformanceCounter"		{ mkL IdVar }
<0>		"QueryPerformanceFrequency"		{ mkL IdVar }
<0>		"SecureZeroMemory"			{ mkL IdVar }
<0>		"WSAAddressToString"			{ mkL IdVar }
<0>		"WSACleanup"				{ mkL IdVar }
<0>		"WSAGetLastError"			{ mkL IdVar }
<0>		"WSAStartup"				{ mkL IdVar }
<0>		"WSAStringToAddress"			{ mkL IdVar }

-- Winapi struct members.
<0>		"GatewayList"				{ mkL IdVar }
<0>		"IpAddressList"				{ mkL IdVar }
<0>		"IpAddress"				{ mkL IdVar }
<0>		"IpMask"				{ mkL IdVar }
<0>		"Next"					{ mkL IdVar }
<0>		"QuadPart"				{ mkL IdVar }
<0>		"String"				{ mkL IdVar }

-- Windows typedefs.
<0>		"DWORD"					{ mkL IdStdType }
<0>		"FILETIME"				{ mkL IdStdType }
<0>		"INT"					{ mkL IdStdType }
<0>		"LPSOCKADDR"				{ mkL IdStdType }
<0>		"IP_ADAPTER_INFO"			{ mkL IdStdType }
<0>		"LPTSTR"				{ mkL IdStdType }
<0>		"u_long"				{ mkL IdStdType }
<0>		"LARGE_INTEGER"				{ mkL IdStdType }
<0>		"SOCKET"				{ mkL IdStdType }
<0>		"WSADATA"				{ mkL IdStdType }

-- System struct types.
<0>		"addrinfo"				{ mkL IdSueType }
<0>		"ifconf"				{ mkL IdSueType }
<0>		"ifreq"					{ mkL IdSueType }
<0>		"epoll_event"				{ mkL IdSueType }
<0>		"ev_async"				{ mkL IdSueType }
<0>		"ev_io"					{ mkL IdSueType }
<0>		"ev_loop"				{ mkL IdSueType }
<0>		"fd_set"				{ mkL IdSueType }
<0>		"in_addr"				{ mkL IdSueType }
<0>		"in6_addr"				{ mkL IdSueType }
<0>		"ipv6_mreq"				{ mkL IdSueType }
<0>		"sockaddr"				{ mkL IdSueType }
<0>		"sockaddr_in"				{ mkL IdSueType }
<0>		"sockaddr_in6"				{ mkL IdSueType }
<0>		"sockaddr_storage"			{ mkL IdSueType }
<0>		"timespec"				{ mkL IdSueType }
<0>		"timeval"				{ mkL IdSueType }

-- Msgpack struct types.
<0>		"msgpack_iovec"				{ mkL IdSueType }
<0>		"msgpack_object_array"			{ mkL IdSueType }
<0>		"msgpack_object_bin"			{ mkL IdSueType }
<0>		"msgpack_object_ext"			{ mkL IdSueType }
<0>		"msgpack_object_kv"			{ mkL IdSueType }
<0>		"msgpack_object_map"			{ mkL IdSueType }
<0>		"msgpack_object"			{ mkL IdSueType }
<0>		"msgpack_object_str"			{ mkL IdSueType }
<0>		"msgpack_object_type"			{ mkL IdSueType }
<0>		"msgpack_packer"			{ mkL IdSueType }
<0>		"msgpack_packer_write"			{ mkL IdFuncType }
<0>		"msgpack_sbuffer"			{ mkL IdSueType }
<0>		"msgpack_timestamp"			{ mkL IdSueType }
<0>		"msgpack_unpacked"			{ mkL IdSueType }
<0>		"msgpack_unpacker"			{ mkL IdSueType }
<0>		"msgpack_unpack_return"			{ mkL IdSueType }
<0>		"msgpack_vrefbuffer"			{ mkL IdSueType }
<0>		"msgpack_zbuffer"			{ mkL IdSueType }
<0>		"msgpack_zone"				{ mkL IdSueType }

-- Sodium types.
<0>		"crypto_generichash_blake2b_state"	{ mkL IdSueType }

-- Sodium constants.
<0,ppSC>	"crypto_"[a-z0-9_]+[A-Z][A-Z0-9_]*	{ mkL IdConst }

-- Clang nullability qualifiers.
<0,ppSC>	"_Nonnull"				{ mkL KwNonnull }
<0,ppSC>	"_Nullable"				{ mkL KwNullable }

-- Standard C (ish).
<ppSC>		defined					{ mkL PpDefined }
<ppSC>		\"[^\"]*\"				{ mkL LitString }
<ppSC>		\n					{ mkL PpNewline `andBegin` 0 }
<ppSC>		\\\n					;
<ppSC>		" "					;
<ppSC>		$white					{ mkE ErrorToken }

<ignoreSC>	"//!TOKSTYLE+"				{ mkL IgnEnd `andBegin` 0 }
<ignoreSC>	([^\/]+|.|\n)				{ mkL IgnBody }

<0,ppSC>	"//"\n					;
<0,ppSC>	"// ".*					;
<0>		[\ \n]+					;
<0>		$white					{ mkE ErrorToken }
<0>		"//!TOKSTYLE-"				{ mkL IgnStart `andBegin` ignoreSC }
<0>		"/*!"					{ mkL CmtStartCode }
<0>		"*/"					{ mkL CmtEnd }
<0>		"/*"					{ mkL CmtStart `andBegin` cmtSC }
<0>		"/**"					{ mkL CmtStartDoc `andBegin` cmtSC }
<0>		"/** @{"				{ mkL CmtStartDocSection `andBegin` cmtSC }
<0>		"/** @} */"				{ mkL CmtEndDocSection }
<0>		"/**""*"+				{ mkL CmtStartBlock `andBegin` cmtSC }
<0,cmtSC>	\"(\\.|[^\"])*\"			{ mkL LitString }
<0>		'(\\|[^'])*'				{ mkL LitChar }
<0>		"<"[a-z0-9\.\/_]+">"			{ mkL LitSysInclude }
<0>		"#if"					{ mkL PpIf `andBegin` ppSC }
<0>		"#ifdef"				{ mkL PpIfdef }
<0>		"#ifndef"				{ mkL PpIfndef }
<0>		"#elif"					{ mkL PpElif `andBegin` ppSC }
<0>		"#else"					{ mkL PpElse }
<0>		"#endif"				{ mkL PpEndif }
<0>		"#define"				{ mkL PpDefine `andBegin` ppSC }
<0>		"#undef"				{ mkL PpUndef }
<0>		"#include"				{ mkL PpInclude }
<0,ppSC>	"tox_"?"bitwise"			{ mkL KwBitwise }
<0,ppSC>	"tox_"?"force"				{ mkL KwForce }
<0,ppSC>	"tox_"?"owner"				{ mkL KwOwner }
<0,ppSC>	"_Owner"				{ mkL KwOwner }
<0,ppSC>	"_Owned"				{ mkL KwOwner }
<0,ppSC>	"break"					{ mkL KwBreak }
<0,ppSC>	"case"					{ mkL KwCase }
<0,ppSC>	"const"					{ mkL KwConst }
<0,ppSC>	"continue"				{ mkL KwContinue }
<0,ppSC>	"default"				{ mkL KwDefault }
<0,ppSC>	"do"					{ mkL KwDo }
<0,ppSC>	"else"					{ mkL KwElse }
<0,ppSC>	"enum"					{ mkL KwEnum }
<0,ppSC>	"extern"				{ mkL KwExtern }
<0,ppSC>	"for"					{ mkL KwFor }
<0,ppSC>	"goto"					{ mkL KwGoto }
<0,ppSC>	"if"					{ mkL KwIf }
<0,ppSC>	"return"				{ mkL KwReturn }
<0,ppSC>	"sizeof"				{ mkL KwSizeof }
<0,ppSC>	"static"				{ mkL KwStatic }
<0,ppSC>	"static_assert"				{ mkL KwStaticAssert }
<0,ppSC>	"struct"				{ mkL KwStruct }
<0,ppSC>	"switch"				{ mkL KwSwitch }
<0,ppSC>	"typedef"				{ mkL KwTypedef }
<0,ppSC>	"union"					{ mkL KwUnion }
<0,ppSC>	"void"					{ mkL KwVoid }
<0,ppSC>	"while"					{ mkL KwWhile }
<0,ppSC>	"bool"					{ mkL IdStdType }
<0,ppSC>	"char"					{ mkL IdStdType }
<0,ppSC>	"double"				{ mkL IdStdType }
<0,ppSC>	"float"					{ mkL IdStdType }
<0,ppSC>	"int"					{ mkL IdStdType }
<0,ppSC>	"long int"				{ mkL IdStdType }
<0,ppSC>	"long signed int"			{ mkL IdStdType }
<0,ppSC>	"long"					{ mkL IdStdType }
<0,ppSC>	"signed int"				{ mkL IdStdType }
<0,ppSC>	"unsigned int"				{ mkL IdStdType }
<0,ppSC>	"unsigned long"				{ mkL IdStdType }
<0,ppSC>	"unsigned long long"			{ mkL IdStdType }
<0,ppSC>	"unsigned"				{ mkL IdStdType }
<0,ppSC>	"va_list"				{ mkL IdStdType }
<0,ppSC>	"false"					{ mkL LitFalse }
<0,ppSC>	"true"					{ mkL LitTrue }
<0,ppSC>	"__func__"				{ mkL IdVar }
<0,ppSC>	"nullptr"				{ mkL IdConst }
<0,ppSC>	"__"[a-zA-Z]+"__"?			{ mkL IdConst }
<0,ppSC>	[A-Z][A-Z0-9_]{1,2}			{ mkL IdSueType }
<0,ppSC>	_*[A-Z][A-Z0-9_]*			{ mkL IdConst }
<0,ppSC>	[A-Z][A-Za-z0-9_]*[a-z][A-Za-z0-9_]*	{ mkL IdSueType }
<0,ppSC>	"cmp_"[a-z][a-z0-9_]*_[suet]		{ mkL IdSueType }
<0,ppSC>	[a-z][a-z0-9_]*_t			{ mkL IdStdType }
<0,ppSC>	[a-z][a-z0-9_]*_cb			{ mkL IdFuncType }
<0,ppSC>	"cmp_"("reader"|"writer"|"skipper")	{ mkL IdFuncType }
<0,ppSC>	[a-z][A-Za-z0-9_]*			{ mkL IdVar }
<0,ppSC,cmtSC>	[0-9]+[LU]*				{ mkL LitInteger }
<0,ppSC,cmtSC>	[0-9]+"."[0-9]+[Ff]?			{ mkL LitFloat }
<0,ppSC>	0x[0-9a-fA-F]+[LU]*			{ mkL LitInteger }
<0,ppSC,cmtSC>	"="					{ mkL PctEq }
<0,ppSC,cmtSC>	"=="					{ mkL PctEqEq }
<0,ppSC>	"&"					{ mkL PctAmpersand }
<0,ppSC>	"&&"					{ mkL PctAmpersandAmpersand }
<0,ppSC>	"&="					{ mkL PctAmpersandEq }
<0,ppSC>	"->"					{ mkL PctArrow }
<0,ppSC,cmtSC>	","					{ mkL PctComma }
<0,ppSC,cmtSC>	"+"					{ mkL PctPlus }
<0,ppSC>	"++"					{ mkL PctPlusPlus }
<0,ppSC>	"+="					{ mkL PctPlusEq }
<0,ppSC,cmtSC>	"-"					{ mkL PctMinus }
<0,ppSC>	"--"					{ mkL PctMinusMinus }
<0,ppSC>	"-="					{ mkL PctMinusEq }
<0,ppSC>	"~"					{ mkL PctTilde }
<0,ppSC,cmtSC>	"/"					{ mkL PctSlash }
<0,ppSC>	"/="					{ mkL PctSlashEq }
<0,ppSC,cmtSC>	"."					{ mkL PctPeriod }
<0,ppSC,cmtSC>	"..."					{ mkL PctEllipsis }
<0,ppSC>	"%"					{ mkL PctPercent }
<0,ppSC>	"%="					{ mkL PctPercentEq }
<0,ppSC,cmtSC>	";"					{ mkL PctSemicolon }
<0,ppSC,cmtSC>	":"					{ mkL PctColon }
<0,ppSC,cmtSC>	"<"					{ mkL PctLess }
<0,ppSC>	"<<"					{ mkL PctLessLess }
<0,ppSC>	"<<="					{ mkL PctLessLessEq }
<0,ppSC>	"<="					{ mkL PctLessEq }
<0,ppSC,cmtSC>	">"					{ mkL PctGreater }
<0,ppSC>	">>"					{ mkL PctGreaterGreater }
<0,ppSC>	">>="					{ mkL PctGreaterGreaterEq }
<0,ppSC,cmtSC>	">="					{ mkL PctGreaterEq }
<0,ppSC>	"|"					{ mkL PctPipe }
<0,ppSC>	"||"					{ mkL PctPipePipe }
<0,ppSC>	"|="					{ mkL PctPipeEq }
<0,ppSC>	"["					{ mkL PctLBrack }
<0,ppSC>	"]"					{ mkL PctRBrack }
<0,ppSC>	"{"					{ mkL PctLBrace }
<0,ppSC>	"}"					{ mkL PctRBrace }
<0,ppSC,cmtSC>	"("					{ mkL PctLParen }
<0,ppSC,cmtSC>	")"					{ mkL PctRParen }
<0,ppSC,cmtSC>	"?"					{ mkL PctQMark }
<0,ppSC,cmtSC>	"!"					{ mkL PctEMark }
<0,ppSC,cmtSC>	"!="					{ mkL PctEMarkEq }
<0,ppSC>	"*"					{ mkL PctAsterisk }
<0,ppSC>	"*="					{ mkL PctAsteriskEq }
<0,ppSC>	"^"					{ mkL PctCaret }
<0,ppSC>	"^="					{ mkL PctCaretEq }

-- Comments.
<cmtSC>		"Copyright ©"				{ mkL CmtSpdxCopyright }
<cmtSC>		"SPDX-License-Identifier:"		{ mkL CmtSpdxLicense }
<cmtSC>		"GPL-3.0-or-later"			{ mkL CmtWord }
<cmtSC>		"TODO("[^\)]+"):"			{ mkL CmtWord }
<cmtSC>		[0-2][0-9](":"[0-5][0-9]){2}"."[0-9]{3}	{ mkL CmtWord }
<cmtSC>		"v"?[0-9]+("."[0-9]+)+			{ mkL CmtWord }
<cmtSC>		[A-Z][A-Za-z]+"::"[a-z_]+		{ mkL CmtWord }
<cmtSC>		([a-z]+"/")+[A-Za-z]+("."[a-z_]+)+	{ mkL CmtWord }
<cmtSC>		[a-z]+("."[a-z_]+)+			{ mkL CmtWord }
<cmtSC>		[a-z]+("-"[a-z_]+)+			{ mkL CmtWord }
<cmtSC>		"@code"					{ mkL CmtCode `andBegin` codeSC }
<cmtSC>		"<code>"				{ mkL CmtCode `andBegin` codeSC }
<cmtSC>		"["[^\]]+"]"				{ mkL CmtAttr }
<cmtSC>		[@\\][a-z_]+				{ mkL CmtCommand }
<cmtSC>		"*"[A-Za-z][A-Za-z0-9_']*"*"		{ mkL CmtWord }
<cmtSC>		"#"[A-Za-z][A-Za-z0-9_]*		{ mkL CmtRef }
<cmtSC>		"_"*[A-Za-z][A-Za-z0-9_']*		{ mkL CmtWord }
<cmtSC>		"#"[0-9]+				{ mkL CmtWord }
<cmtSC>		"http://"[^\ ]+				{ mkL CmtWord }
<cmtSC>		[0-9]+"%"				{ mkL LitInteger }
<cmtSC>		"`"([^`]|"\`")+"`"			{ mkL CmtCode }
<cmtSC>		"${"([^\}])+"}"				{ mkL CmtCode }
<cmtSC>		"-"+					{ mkL CmtWord }
<cmtSC>		[&\|]+					{ mkL CmtWord }
<cmtSC>		"–"					{ mkL CmtWord }
<cmtSC>		"*/"					{ mkL CmtEnd `andBegin` 0 }
<cmtSC>		\n					{ mkL PpNewline `andBegin` cmtNewlineSC }
<cmtSC>		" "+					{ mkL CmtSpace }

<cmtNewlineSC>	" "+"*"+"/"				{ mkL CmtEnd `andBegin` 0 }
<cmtNewlineSC>	" "+"*"					{ begin cmtSC }

<codeStartSC>	" "+					{ mkL CmtSpace `andBegin` codeSC }
<codeStartSC>	\n					{ mkL PpNewline `andBegin` codeNewlineSC }

<codeNewlineSC>	" "+"*"					{ begin codeStartSC }

-- <code></code> blocks in comments.
<codeSC>	"@endcode"				{ mkL CmtCode `andBegin` cmtSC }
<codeSC>	"</code>"				{ mkL CmtCode `andBegin` cmtSC }
<codeSC>	\n					{ mkL PpNewline `andBegin` codeNewlineSC }
<codeSC>	[^@\<]+					{ mkL CmtCode }

-- Error handling.
<0,ppSC,cmtSC,codeSC>	.				{ mkL ErrorToken }

{
deriving instance Generic AlexPosn
instance FromJSON AlexPosn
instance FromJSONKey AlexPosn where fromJSONKey = Aeson.FromJSONKeyText (read . Text.unpack)
instance ToJSON AlexPosn
instance ToJSONKey AlexPosn where toJSONKey = Aeson.toJSONKeyText (Text.pack . show)
instance Hashable AlexPosn
deriving instance Read AlexPosn

instance Arbitrary AlexPosn where
    arbitrary = AlexPn <$> arbitrary <*> arbitrary <*> arbitrary

data Lexeme text = L AlexPosn LexemeClass text
    deriving (Eq, Show, Read, Generic, Functor, Foldable, Traversable, Ord)

instance Arbitrary text => Arbitrary (Lexeme text) where
    arbitrary = L <$> arbitrary <*> arbitrary <*> arbitrary


instance FromJSON text => FromJSON (Lexeme text)
instance ToJSON text => ToJSON (Lexeme text)
instance Hashable text => Hashable (Lexeme text) where
    hashWithSalt s (L p c t) = s `hashWithSalt` p `hashWithSalt` c `hashWithSalt` t

mkL :: LexemeClass -> AlexInput -> Int64 -> Alex (Lexeme Text)
mkL c (p, _, str, _) len = pure $ L p c (piece str)
  where piece = Text.decodeUtf8 . LBS.toStrict . LBS.take len

mkE :: LexemeClass -> AlexInput -> Int64 -> Alex (Lexeme Text)
mkE c (p, _, str, _) len = alexError $ ": " <> show (L p c (piece str))
  where piece = Text.decodeUtf8 . LBS.toStrict . LBS.take len

lexemePosn :: Lexeme text -> AlexPosn
lexemePosn (L p _ _) = p

lexemeClass :: Lexeme text -> LexemeClass
lexemeClass (L _ c _) = c

lexemeText :: Lexeme text -> text
lexemeText (L _ _ s) = s

lexemeLine :: Lexeme text -> Int
lexemeLine (L (AlexPn _ l _) _ _) = l

alexEOF :: Alex (Lexeme Text)
alexEOF = return (L (AlexPn 0 0 0) Eof Text.empty)

data Context
    = Context String
    | ContextLexeme String (Lexeme Text)
    | ContextNode String (Node (Lexeme Text))
    deriving (Eq, Show, Read, Generic)

instance FromJSON Context
instance ToJSON Context
instance Hashable Context

data AlexUserState = AlexUserState { alex_context :: [Context] }
alexInitUserState = AlexUserState []

data ParseError = ParseError
    { errorPosn     :: AlexPosn
    , errorContext  :: [Context]
    , errorExpected :: [String]
    , errorLexeme   :: Lexeme Text
    } deriving (Eq, Show, Read, Generic)

instance FromJSON ParseError
instance ToJSON ParseError
instance Hashable ParseError

pushContext :: Context -> Alex ()
pushContext ctx = Alex $ \s ->
    let us = alex_ust s
        us' = us { alex_context = ctx : alex_context us }
    in Right (s { alex_ust = us' }, ())

popContext :: Alex ()
popContext = Alex $ \s ->
    let us = alex_ust s
        us' = us { alex_context = case alex_context us of [] -> []; (_:cs) -> cs }
    in Right (s { alex_ust = us' }, ())

getContext :: Alex [Context]
getContext = Alex $ \s -> Right (s, alex_context (alex_ust s))

alexScanTokens :: LBS.ByteString -> Either String [Lexeme Text]
alexScanTokens str =
    runAlex str $ loop []
  where
    loop toks = do
        tok@(L _ cl _) <- alexMonadScan
        if cl == Eof
            then return $ reverse toks
            else loop $! (tok:toks)
}
