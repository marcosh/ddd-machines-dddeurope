{ pkgs }: {
    deps = [
        (pkgs.haskellPackages.ghcWithPackages (pkgs: [
            QuickCheck
            text
        ]))
        pkgs.haskell-language-server
    ];
}
