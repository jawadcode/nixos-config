{
  lib,
  stdenvNoCC,
  fetchzip,
}:
stdenvNoCC.mkDerivation {
  pname = "iosevka-term-ss07-nerd-font";
  version = "30.1.2";
  src = fetchzip {
    url = "https://github.com/jawadcode/IosevkaTermSS07-Nerd-Font/archive/refs/tags/v30.1.2.zip";
    hash = "sha256-ISMCuHih8GS21pwYAQaVJ4Y/7XKp/e8Cof0rnhjGzHs=";
  };

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/fonts/truetype
    cp patched/IosevkaTermSS07NerdFont-*.ttf $out/share/fonts/truetype
    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://typeof.net/Iosevka/";
    description = "Versatile typeface for code, from code.";
    license = licenses.ofl;
    platform = platforms.all;
    maintainers = [];
  };
}
