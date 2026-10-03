{ lib, applyPatches, buildGramRustExtension, fetchFromGitHub, fetchFromGitLab }:

buildGramRustExtension (finalAttrs: {
  id = "ocaml";
  version = "0.4.1";
  src = fetchFromGitHub {
    owner = "zed-extensions";
    repo = "ocaml";
    tag = "v${finalAttrs.version}";
    hash = "sha256-Y7HYVvX2JPluXz/3fN2ZlPoLSkkb15qkfiEJGTJgLdg=";
  };

  cargoHash = "sha256-kmrAKIKDYgiB7Gwwwauy4pE/FcBmP0W/HUU1sSg+EiY=";

  grammars =
    let
      ocaml = applyPatches {
        name = "tree-sitter-ocaml-patched";
        src = fetchFromGitHub {
          name = "tree-sitter-ocaml";
          owner = "tree-sitter";
          repo = "tree-sitter-ocaml";
          rev = "0b12614ded3ec7ed7ab7933a9ba4f695ba4c342e";
          hash = "sha256-ysMYLTIhU4jN24cPH0J8v9685ED+OQU6x/pLBeHXeYQ=";
        };
        postPatch = ''
          	  for grammar in ocaml interface; do
          	    cp -R include grammars/$grammar/include
          	    substituteInPlace grammars/$grammar/src/scanner.c \
          	      --replace-fail '../../../include/scanner.h' '../include/scanner.h'
          	    substituteInPlace grammars/$grammar/src/parser.c \
          		--replace-fail 'tree_sitter/parser.h' '../include/tree_sitter/parser.h'
          	  done
        '';
      };
    in
    {
      ocaml = "${ocaml}/grammars/ocaml";
      ocaml_interface = "${ocaml}/grammars/interface";
      reason = fetchFromGitHub {
        name = "tree-sitter-reason";
        owner = "reasonml-editor";
        repo = "tree-sitter-reason";
        rev = "0226cfecaa2257d56d7c3f364f789cba042e403f";
        hash = "sha256-h/S+igTPPLHA0XoIfXk7fQvy/ImZ9RYpBdwsKE6xXzM=";
      };
      dune = fetchFromGitHub {
        name = "tree-sitter-dune";
        owner = "WHForks";
        repo = "tree-sitter-dune";
        rev = "b3f7882e1b9a1d8811011bf6f0de1c74c9c93949";
        hash = "sha256-D2utYHxwBakKe7sKKHMj2yz42p9GTF3FBW+iX2rIEwc=";
      };
      menhir = fetchFromGitHub {
        name = "tree-sitter-menhir";
        owner = "Kerl13";
        repo = "tree-sitter-menhir";
        rev = "be8866a6bcc2b563ab0de895af69daeffa88fe70";
        hash = "sha256-CQVEQurf8Ur5xnz+g7e1nck0a32o4oeMOT78thjx8MQ=";
      };
      ocaml_mlx =
        let
          ocaml-mlx = applyPatches {
            name = "tree-sitter-mlx-patched";
            src = fetchFromGitHub {
              name = "tree-sitter-mlx";
              owner = "ocaml-mlx";
              repo = "tree-sitter-mlx";
              rev = "fbe904bbf55be15d34b57ba9c45750fc5187960f";
              hash = "sha256-oSaQ51d8u+1OH7A+p038n+g0r8sK2T08tC3iEcQL6BM=";
            };
            postPatch = ''
              	      cp -R common grammars/mlx/common
              	      substituteInPlace grammars/mlx/src/scanner.c \
              		--replace-fail '../../../common/scanner.h' '../common/scanner.h'
              	    '';
          };
        in
        "${ocaml-mlx}/grammars/mlx";
      ocamllex = fetchFromGitHub {
        name = "tree-sitter-ocamllex";
        owner = "314eter";
        repo = "tree-sitter-ocamllex";
        rev = "c5cf996c23e38a1537069fbe2d4bb83a75fc7b2f";
        hash = "sha256-eDJRTLYKHcL7yAgFL8vZQh9zp5fBxcZRsWChp8y3Am0=";
      };
      odoc_mld = fetchFromGitHub {
        name = "tree-sitter-odoc-mld";
        owner = "manenko";
        repo = "tree-sitter-odoc-mld";
        rev = "2eafb2a4732b2029d03534ccb0df9134b2577161";
        hash = "sha256-1v7p3VqvnOJT6TUHzL95v6Hl5zL7lWydgZAEFAUxM58=";
      };
    };
  meta = {
    description = "Zed Editor OCaml Support";
    homepage = "https://github.com/zed-extensions/ocaml";
    maintainers = [ "jawadcode" ];
  };
})
