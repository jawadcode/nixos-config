{ lib, buildGramRustExtension, fetchFromGitHub, fetchFromGitLab }:

buildGramRustExtension (finalAttrs: {
  id = "haskell";
  version = "1.0.1";
  src = fetchFromGitHub
    {
      owner = "zed-extensions";
      repo = "haskell";
      tag = "v${finalAttrs.version}";
      hash = "sha256-JsQK/yWxM1sUiYOlFgbo0HX81jdRsXFRDVBaByE2ZxQ=";
    };

  cargoHash = "sha256-zg/5sfGesqhAW15PsZ9KsqfiN+AzH2gjw3sT5gsc56k=";

  grammars = {
    haskell = fetchFromGitHub {
      owner = "tree-sitter";
      repo = "tree-sitter-haskell";
      rev = "c30d812bc90827f1a54106a25bc9a6307f5cdcec"; # 0.23.1
      hash = "sha256-bggXKbV4vTWapQAbERPUszxpQtpC1RTujNhwgbjY7T4=";
    };
    cabal = fetchFromGitLab {
      owner = "zweimach";
      repo = "tree-sitter-cabal";
      rev = "6f00f6d4883eb2eb650eea7cc1e95bd25e48419c";
      hash = "sha256-OLgnlhI/wBE5BBA+SNL3WA4+pNVCwo3+xY93DmvV6FQ=";
    };
    haskell_literate = fetchFromGitHub {
      owner = "LaurentRDC";
      repo = "tree-sitter-haskell-literate";
      rev = "8ad7bd1b1595f4cc1a4ccc775d4a3c460f43a596";
      hash = "sha256-P9FTcMwPU1AvH55ly0XydCSVOP0Mfi6jQ0Yg6Muraqk=";
    };
    alex = fetchFromGitHub {
      owner = "brandonchinn178";
      repo = "tree-sitter-alex";
      rev = "1b20e2e6592ccd859ae5e06649d8335d19cdb60c";
      hash = "sha256-ea+eRbUJRAbHj+voAEjmeRtLCUD2bcwSAaNwWA8O3HE=";
    };
  };
  meta = {
    description = "Zed Editor Haskell Support";
    homepage = "https://github.com/zed-extensions/haskell";
    maintainers = [ "jawadcode" ];
  };
})
