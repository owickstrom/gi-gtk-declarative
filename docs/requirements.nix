# generated using pypi2nix tool (version: 2.0.1)
# See more at: https://github.com/nix-community/pypi2nix
#
# COMMAND:
#   pypi2nix -V python37 -r requirements.txt
#

{ pkgs ? import ../nixpkgs.nix,
  overrides ? ({ pkgs, python }: self: super: {})
}:

let

  inherit (pkgs) makeWrapper;
  inherit (pkgs.stdenv.lib) fix' extends inNixShell;

  pythonPackages = pkgs.lib.makeScope pkgs.newScope (
    import "${toString pkgs.path}/pkgs/top-level/python-packages.nix" {
      inherit pkgs;
      inherit (pkgs) stdenv lib;
      python = pkgs.python37;
    }
  );

  commonBuildInputs = [];
  commonDoCheck = false;

  withPackages = pkgs':
    let
      pkgs = builtins.removeAttrs pkgs' ["__unfix__"];
      interpreterWithPackages = selectPkgsFn: pythonPackages.buildPythonPackage {
        name = "python37-interpreter";
        buildInputs = [ makeWrapper ] ++ (selectPkgsFn pkgs);
        buildCommand = ''
          mkdir -p $out/bin
          ln -s ${pythonPackages.python.interpreter} \
              $out/bin/${pythonPackages.python.executable}
          for dep in ${builtins.concatStringsSep " "
              (selectPkgsFn pkgs)}; do
            if [ -d "$dep/bin" ]; then
              for prog in "$dep/bin/"*; do
                if [ -x "$prog" ] && [ -f "$prog" ]; then
                  ln -s $prog $out/bin/`basename $prog`
                fi
              done
            fi
          done
          for prog in "$out/bin/"*; do
            wrapProgram "$prog" --prefix PYTHONPATH : "$PYTHONPATH"
          done
          pushd $out/bin
          ln -s ${pythonPackages.python.executable} python
          ln -s ${pythonPackages.python.executable} \
              python3
          popd
        '';
        passthru.interpreter = pythonPackages.python;
      };

      interpreter = interpreterWithPackages builtins.attrValues;
    in {
      __old = pythonPackages;
      inherit interpreter;
      inherit interpreterWithPackages;
      mkDerivation = args: pythonPackages.buildPythonPackage (args // {
        nativeBuildInputs = (args.nativeBuildInputs or []) ++ args.buildInputs;
      });
      packages = pkgs;
      overrideDerivation = drv: f:
        pythonPackages.buildPythonPackage (
          drv.drvAttrs // f drv.drvAttrs // { meta = drv.meta; }
        );
      withPackages = pkgs'':
        withPackages (pkgs // pkgs'');
    };

  python = withPackages {};

  generated = self: {
    "click" = python.mkDerivation {
      name = "click-7.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/f8/5c/f60e9d8a1e77005f664b76ff8aeaee5bc05d0a91798afd7f53fc998dbc47/Click-7.0.tar.gz";
        sha256 = "5b94b49521f6456670fdb30cd82a4eca9412788a93fa6dd6df72c94d5a8ff2d7";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://palletsprojects.com/p/click/";
        license = licenses.bsdOriginal;
        description = "Composable command line interface toolkit";
      };
    };

    "htmlmin" = python.mkDerivation {
      name = "htmlmin-0.1.12";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/b3/e7/fcd59e12169de19f0131ff2812077f964c6b960e7c09804d30a7bf2ab461/htmlmin-0.1.12.tar.gz";
        sha256 = "50c1ef4630374a5d723900096a961cff426dff46b48f34d194a81bbe14eca178";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://htmlmin.readthedocs.io/en/latest/";
        license = licenses.bsdOriginal;
        description = "An HTML Minifier";
      };
    };

    "jinja2" = python.mkDerivation {
      name = "jinja2-2.10.3";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/7b/db/1d037ccd626d05a7a47a1b81ea73775614af83c2b3e53d86a0bb41d8d799/Jinja2-2.10.3.tar.gz";
        sha256 = "9fe95f19286cfefaa917656583d020be14e7859c6b0252588391e47db34527de";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."markupsafe"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://palletsprojects.com/p/jinja/";
        license = licenses.bsdOriginal;
        description = "A very fast and expressive template engine.";
      };
    };

    "jsmin" = python.mkDerivation {
      name = "jsmin-2.2.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/17/73/615d1267a82ed26cd7c124108c3c61169d8e40c36d393883eaee3a561852/jsmin-2.2.2.tar.gz";
        sha256 = "b6df99b2cd1c75d9d342e4335b535789b8da9107ec748212706ef7bbe5c2553b";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/tikitu/jsmin/";
        license = licenses.mit;
        description = "JavaScript minifier.";
      };
    };

    "livereload" = python.mkDerivation {
      name = "livereload-2.6.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/27/26/85ba3851d2e4905be7d2d41082adca833182bb1d7de9dfc7f623383d36e1/livereload-2.6.1.tar.gz";
        sha256 = "89254f78d7529d7ea0a3417d224c34287ebfe266b05e67e51facaf82c27f0f66";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."six"
        self."tornado"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/lepture/python-livereload";
        license = licenses.bsdOriginal;
        description = "Python LiveReload is an awesome tool for web developers";
      };
    };

    "markdown" = python.mkDerivation {
      name = "markdown-3.1.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/ac/df/0ae25a9fd5bb528fe3c65af7143708160aa3b47970d5272003a1ad5c03c6/Markdown-3.1.1.tar.gz";
        sha256 = "2e50876bcdd74517e7b71f3e7a76102050edec255b3983403f1a63e7c8a41e7a";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."setuptools"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://Python-Markdown.github.io/";
        license = licenses.bsdOriginal;
        description = "Python implementation of Markdown.";
      };
    };

    "markupsafe" = python.mkDerivation {
      name = "markupsafe-1.1.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/b9/2e/64db92e53b86efccfaea71321f597fa2e1b2bd3853d8ce658568f7a13094/MarkupSafe-1.1.1.tar.gz";
        sha256 = "29872e92839765e546828bb7754a68c418d927cd064fd4708fab9fe9c8bb116b";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://palletsprojects.com/p/markupsafe/";
        license = licenses.bsdOriginal;
        description = "Safely add untrusted strings to HTML/XML markup.";
      };
    };

    "mkdocs" = python.mkDerivation {
      name = "mkdocs-1.0.4";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/7e/58/1ff1580aab41d6247455495a9b5ff649b94cbfc22d63682dc42a5964080e/mkdocs-1.0.4.tar.gz";
        sha256 = "17d34329aad75d5de604b9ed4e31df3a4d235afefdc46ce7b1964fddb2e1e939";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."click"
        self."jinja2"
        self."livereload"
        self."markdown"
        self."pyyaml"
        self."tornado"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://www.mkdocs.org";
        license = licenses.bsdOriginal;
        description = "Project documentation with Markdown.";
      };
    };

    "mkdocs-material" = python.mkDerivation {
      name = "mkdocs-material-4.6.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/91/4d/6f4e0750d154bfe9029a46b8f2acb7a2a26bcb789c97994dbd7eaec69a89/mkdocs-material-4.6.0.tar.gz";
        sha256 = "b21aa2645ccb11442ea381c92d187bbc94127f50702c0d28c3fc0152fa7b29da";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."markdown"
        self."mkdocs"
        self."mkdocs-minify-plugin"
        self."pygments"
        self."pymdown-extensions"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://squidfunk.github.io/mkdocs-material/";
        license = licenses.mit;
        description = "A Material Design theme for MkDocs";
      };
    };

    "mkdocs-minify-plugin" = python.mkDerivation {
      name = "mkdocs-minify-plugin-0.2.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/66/fe/47e0acd1df33a289a075af252f3d7d65fb8acec425f5fac12493669c6828/mkdocs-minify-plugin-0.2.1.tar.gz";
        sha256 = "3000a5069dd0f42f56a8aaf7fd5ea1222c67487949617e39585d6b6434b074b6";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."htmlmin"
        self."jsmin"
        self."mkdocs"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/byrnereese/mkdocs-minify-plugin";
        license = licenses.mit;
        description = "An MkDocs plugin to minify HTML and/or JS files prior to being written to disk";
      };
    };

    "pep562" = python.mkDerivation {
      name = "pep562-1.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/98/46/048232af31a939b3c0e363418faa12f4cc0e144d00cebce6ec9ff5d0f06b/pep562-1.0.tar.gz";
        sha256 = "58cb1cc9ee63d93e62b4905a50357618d526d289919814bea1f0da8f53b79395";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/facelessuser/pep562";
        license = licenses.mit;
        description = "Backport of PEP 562.";
      };
    };

    "pygments" = python.mkDerivation {
      name = "pygments-2.5.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/cb/9f/27d4844ac5bf158a33900dbad7985951e2910397998e85712da03ce125f0/Pygments-2.5.2.tar.gz";
        sha256 = "98c8aa5a9f778fcd1026a17361ddaf7330d1b7c62ae97c3bb0ae73e0b9b6b0fe";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://pygments.org/";
        license = licenses.bsdOriginal;
        description = "Pygments is a syntax highlighting package written in Python.";
      };
    };

    "pymdown-extensions" = python.mkDerivation {
      name = "pymdown-extensions-6.2.1";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/69/9a/3091d70258a440f82dbfa2fb8e877055ffa1203cbd24e14f25aa48c7d2fd/pymdown-extensions-6.2.1.tar.gz";
        sha256 = "3bbe6048275f8a0d13a0fe44e0ea201e67268aa7bb40c2544eef16abbf168f7b";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [
        self."markdown"
        self."pep562"
      ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/facelessuser/pymdown-extensions";
        license = licenses.mit;
        description = "Extension pack for Python Markdown.";
      };
    };

    "pyyaml" = python.mkDerivation {
      name = "pyyaml-5.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/8d/c9/e5be955a117a1ac548cdd31e37e8fd7b02ce987f9655f5c7563c656d5dcb/PyYAML-5.2.tar.gz";
        sha256 = "c0ee8eca2c582d29c3c2ec6e2c4f703d1b7f1fb10bc72317355a746057e7346c";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/yaml/pyyaml";
        license = licenses.mit;
        description = "YAML parser and emitter for Python";
      };
    };

    "setuptools" = python.mkDerivation {
      name = "setuptools-42.0.2";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/f7/b6/5b98441b6749ea1db1e41e5e6e7a93cbdd7ffd45e11fe1b22d45884bc777/setuptools-42.0.2.zip";
        sha256 = "c5b372090d7c8709ce79a6a66872a91e518f7d65af97fca78135e1cb10d4b940";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/pypa/setuptools";
        license = licenses.mit;
        description = "Easily download, build, install, upgrade, and uninstall Python packages";
      };
    };

    "six" = python.mkDerivation {
      name = "six-1.13.0";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/94/3e/edcf6fef41d89187df7e38e868b2dd2182677922b600e880baad7749c865/six-1.13.0.tar.gz";
        sha256 = "30f610279e8b2578cab6db20741130331735c781b56053c59c4076da27f06b66";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "https://github.com/benjaminp/six";
        license = licenses.mit;
        description = "Python 2 and 3 compatibility utilities";
      };
    };

    "tornado" = python.mkDerivation {
      name = "tornado-6.0.3";
      src = pkgs.fetchurl {
        url = "https://files.pythonhosted.org/packages/30/78/2d2823598496127b21423baffaa186b668f73cd91887fcef78b6eade136b/tornado-6.0.3.tar.gz";
        sha256 = "c845db36ba616912074c5b1ee897f8e0124df269468f25e4fe21fe72f6edd7a9";
};
      doCheck = commonDoCheck;
      buildInputs = commonBuildInputs ++ [ ];
      propagatedBuildInputs = [ ];
      meta = with pkgs.stdenv.lib; {
        homepage = "http://www.tornadoweb.org/";
        license = licenses.asl20;
        description = "Tornado is a Python web framework and asynchronous networking library, originally developed at FriendFeed.";
      };
    };
  };
  localOverridesFile = ./requirements_override.nix;
  localOverrides = import localOverridesFile { inherit pkgs python; };
  commonOverrides = [
        (let src = pkgs.fetchFromGitHub { owner = "nix-community"; repo = "pypi2nix-overrides"; rev = "db87933d87d9b3943cf636a49f16b76c9ea66db7"; sha256 = "1phiqh72dyg7qhkv15kdg4gjkx8rkywvs41j7liz5faj66ijlpv6"; } ; in import "${src}/overrides.nix" { inherit pkgs python; })
  ];
  paramOverrides = [
    (overrides { inherit pkgs python; })
  ];
  allOverrides =
    (if (builtins.pathExists localOverridesFile)
     then [localOverrides] else [] ) ++ commonOverrides ++ paramOverrides;

in python.withPackages
   (fix' (pkgs.lib.fold
            extends
            generated
            allOverrides
         )
   )
