{
  nixConfig.bash-prompt-prefix = ''\[\e[0;31m\](haskell) \e[0m'';

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {
    flake-utils,
    nixpkgs,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      ghcVersion = "ghc98";
      hsPkgs = pkgs.haskell.packages.${ghcVersion};
    in {
      devShells.default = pkgs.mkShell {
        packages = with hsPkgs; [
          ghc
          cabal-install
          hoogle
          fourmolu
          haskell-language-server
        ];

        shellHook = ''echo henlo wrd :\)'';
      };
    });
}
