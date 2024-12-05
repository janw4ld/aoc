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
      ghcVersion = "ghc9101";
      hsPkgs = pkgs.haskell.packages.${ghcVersion};
    in {
      devShells.default = pkgs.mkShell {
        packages =
          (with hsPkgs; [
            ghc
            haskell-language-server
            fourmolu
            hoogle
          ])
          ++ (with pkgs; [
            hlint
            cabal-install
          ]);

        shellHook = ''echo henlo wrd :\)'';
      };
    });
}
