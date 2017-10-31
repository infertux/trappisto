module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Json.Decode as Decode
import Components.Block as Block
import Components.Transaction as Transaction


all : Test
all =
    describe "Unit tests"
        [ describe "Block"
            [ test "decodeGetBlockHash" <|
                \() ->
                    Expect.equal (Ok "deadbeef") <| Decode.decodeString Block.decodeGetBlockHash "{\"result\":\"deadbeef\"}"
            ]
        , describe "Transaction"
            [ test "decodeGetRawTransaction" <|
                \() ->
                    let
                        result =
                            Decode.decodeString Transaction.decodeGetRawTransaction getRawTransactionFixture

                        model =
                            case result of
                                Ok jsonModel ->
                                    jsonModel |> Transaction.modelFromJson

                                Err error ->
                                    Debug.crash <| toString error
                    in
                        Expect.equal 452 model.size
            ]
        , describe "Fuzz test examples, using randomly generated input"
            -- XXX: keping them as examples
            [ fuzz (list int) "Lists always have positive length" <|
                \aList ->
                    List.length aList |> Expect.atLeast 0
            , fuzz (list int) "Sorting a list does not change its length" <|
                \aList ->
                    List.sort aList |> List.length |> Expect.equal (List.length aList)
            , fuzzWith { runs = 1000 } int "List.member will find an integer in a list containing it" <|
                \i ->
                    List.member i [ i ] |> Expect.true "If you see this, List.member returned False!"
            , fuzz2 string string "The length of a string equals the sum of its substrings' lengths" <|
                \s1 s2 ->
                    s1 ++ s2 |> String.length |> Expect.equal (String.length s1 + String.length s2)
            ]
        ]


getRawTransactionFixture : String
getRawTransactionFixture =
    """
{
  "result": {
    "hex": "010000000258bfcccc818221efefc5e84606ec0e03f52cbe9a656a92edb9d46e48227484990300000001ffffffff5c1cde6cdfe165d617ba425cbba35a2bbed141a21a707f128c454e08c72946660200000000ffffffff03bd1002000000000000001976a91453a6cd89247a125cd40ea2bdd28dc8dc1750825f88ace9c9fd060200000000001976a91453a6cd89247a125cd40ea2bdd28dc8dc1750825f88ac8e10607e0000000000001976a91489a7c60e95db6bfc65254073567a5b5fded367b788ac000000000000000002821cafae01000000b8c50200020000006a473044022008eafe82ef886d54a4693e9573d106fa6f7ca664e3843924eb415275f04547d1022043bae7f4d4611e88448f32b60e872f89fd38f46f7ebfa3bc93342cbbdbd9d1fc012102badaf6f911d76fbabfbaf069ea996219719c8938229974c7a818f2ee01c4231662b1bed60000000029c70200010000006b483045022100bc56b841e188632e1e0f507075be105616d7d910dc39dc3f7e9a2b627809c65802204051ce88c7a06e23101bbf9d29ca599bbc20c743ca210a5a84cd8122a8e22986012103fcda5a2f5c5277671a7f0c8d384fde4894245d5e7b59469ac551d87c1c3cf3ce",
    "txid": "71fec791fcc68781d6a747ebb3f04b9b395b7884396f75788660e165bd08dc01",
    "version": 1,
    "locktime": 0,
    "expiry": 0,
    "vin": [
      {
        "txid": "99847422486ed4b9ed926a659abe2cf5030eec0646e8c5efef218281ccccbf58",
        "vout": 3,
        "tree": 1,
        "sequence": 4294967295,
        "amountin": 72.25678978,
        "blockheight": 181688,
        "blockindex": 2,
        "scriptSig": {
          "asm": "3044022008eafe82ef886d54a4693e9573d106fa6f7ca664e3843924eb415275f04547d1022043bae7f4d4611e88448f32b60e872f89fd38f46f7ebfa3bc93342cbbdbd9d1fc01 02badaf6f911d76fbabfbaf069ea996219719c8938229974c7a818f2ee01c42316",
          "hex": "473044022008eafe82ef886d54a4693e9573d106fa6f7ca664e3843924eb415275f04547d1022043bae7f4d4611e88448f32b60e872f89fd38f46f7ebfa3bc93342cbbdbd9d1fc012102badaf6f911d76fbabfbaf069ea996219719c8938229974c7a818f2ee01c42316"
        }
      },
      {
        "txid": "664629c7084e458c127f701aa241d1be2b5aa3bb5c42ba17d665e1df6cde1c5c",
        "vout": 2,
        "tree": 0,
        "sequence": 4294967295,
        "amountin": 36.02821474,
        "blockheight": 182057,
        "blockindex": 1,
        "scriptSig": {
          "asm": "3045022100bc56b841e188632e1e0f507075be105616d7d910dc39dc3f7e9a2b627809c65802204051ce88c7a06e23101bbf9d29ca599bbc20c743ca210a5a84cd8122a8e2298601 03fcda5a2f5c5277671a7f0c8d384fde4894245d5e7b59469ac551d87c1c3cf3ce",
          "hex": "483045022100bc56b841e188632e1e0f507075be105616d7d910dc39dc3f7e9a2b627809c65802204051ce88c7a06e23101bbf9d29ca599bbc20c743ca210a5a84cd8122a8e22986012103fcda5a2f5c5277671a7f0c8d384fde4894245d5e7b59469ac551d87c1c3cf3ce"
        }
      }
    ],
    "vout": [
      {
        "value": 0.00135357,
        "n": 0,
        "version": 0,
        "scriptPubKey": {
          "asm": "OP_DUP OP_HASH160 53a6cd89247a125cd40ea2bdd28dc8dc1750825f OP_EQUALVERIFY OP_CHECKSIG",
          "hex": "76a91453a6cd89247a125cd40ea2bdd28dc8dc1750825f88ac",
          "reqSigs": 1,
          "type": "pubkeyhash",
          "addresses": [
            "DsYbDQ256aLuFh7cbLu9NYAFpwpeEyEXiVA"
          ]
        }
      },
      {
        "value": 87.07230185,
        "n": 1,
        "version": 0,
        "scriptPubKey": {
          "asm": "OP_DUP OP_HASH160 53a6cd89247a125cd40ea2bdd28dc8dc1750825f OP_EQUALVERIFY OP_CHECKSIG",
          "hex": "76a91453a6cd89247a125cd40ea2bdd28dc8dc1750825f88ac",
          "reqSigs": 1,
          "type": "pubkeyhash",
          "addresses": [
            "DsYbDQ256aLuFh7cbLu9NYAFpwpeEyEXiVA"
          ]
        }
      },
      {
        "value": 21.2022491,
        "n": 2,
        "version": 0,
        "scriptPubKey": {
          "asm": "OP_DUP OP_HASH160 89a7c60e95db6bfc65254073567a5b5fded367b7 OP_EQUALVERIFY OP_CHECKSIG",
          "hex": "76a91489a7c60e95db6bfc65254073567a5b5fded367b788ac",
          "reqSigs": 1,
          "type": "pubkeyhash",
          "addresses": [
            "DsdWm3q4mpvq26gfLL4Xr1d2UQi4qWH1vps"
          ]
        }
      }
    ],
    "blockhash": "00000000000000500c81f741ff0e55cf1f40c8ec761e0d69c64af08768fdb600",
    "blockheight": 182085,
    "confirmations": 15,
    "time": 1509434441,
    "blocktime": 1509434441
  },
  "error": null,
  "id": "0"
}
"""
