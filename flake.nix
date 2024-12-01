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
    in {
      devShells.default = pkgs.mkShell {
        packages = with pkgs; [
          ghc
          cabal-install
          haskell-language-server
          haskellPackages.fourmolu
          hlint
          haskellPackages.hoogle
        ];

        shellHook = ''echo henlo wrd :\)'';
      };
    });
}
