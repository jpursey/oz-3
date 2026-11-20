# Copyright (c) 2026 John Pursey
#
# Use of this source code is governed by an MIT-style License that can be found
# in the LICENSE file or at https://opensource.org/licenses/MIT.

# The devcontainer's image (see devcontainer.json): Ubuntu 24.04 with everything
# a Linux build of OZ-3 and Game Bits needs, including the Vulkan SDK, whose
# environment is set up for every process, as the SDK's setup-env.sh would. It
# is Game Bits' devcontainer image with OZ-3's clang-format added. It needs an
# x86_64 host, as the SDK is only built for x86_64 Linux.
FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04

# Game Bits' own scripts install what its Linux build needs, from its main,
# which OZ-3 builds against. Game Bits isn't checked out yet when the image is
# built (devcontainer.json clones it later), so they are downloaded. Docker
# reuses each step until its script changes.
ADD https://raw.githubusercontent.com/jpursey/game-bits/main/tools/install-linux-packages.sh /tmp/gb-tools/
RUN bash /tmp/gb-tools/install-linux-packages.sh && rm -rf /var/lib/apt/lists/*

ARG VULKAN_SDK_VERSION
ADD https://raw.githubusercontent.com/jpursey/game-bits/main/tools/install-vulkan-sdk.sh /tmp/gb-tools/
RUN bash /tmp/gb-tools/install-vulkan-sdk.sh /opt/vulkan-sdk "${VULKAN_SDK_VERSION}"
ENV VULKAN_SDK=/opt/vulkan-sdk/${VULKAN_SDK_VERSION}/x86_64
ENV PATH=${VULKAN_SDK}/bin:${PATH} \
  PKG_CONFIG_PATH=${VULKAN_SDK}/lib/VulkanLoader/lib/pkgconfig:${VULKAN_SDK}/share/pkgconfig:${VULKAN_SDK}/lib/pkgconfig \
  CMAKE_PREFIX_PATH=${VULKAN_SDK}:${VULKAN_SDK}/lib/VulkanLoader

# Programs find the SDK's Vulkan loader and validation layer where the system's
# would be, rather than through the environment setup-env.sh sets. So they work
# however they are started, including from the desktop, which desktop-lite
# starts through sudo, clearing the environment.
RUN echo "${VULKAN_SDK}/lib/VulkanLoader/lib" > /etc/ld.so.conf.d/000-vulkan-sdk.conf \
  && ldconfig \
  && mkdir -p /usr/local/share/vulkan/explicit_layer.d \
  && sed "s#\"library_path\": \"[^\"]*\"#\"library_path\": \"${VULKAN_SDK}/lib/libVkLayer_khronos_validation.so\"#" \
    "${VULKAN_SDK}/share/vulkan/explicit_layer.d/VkLayer_khronos_validation.json" \
    > /usr/local/share/vulkan/explicit_layer.d/VkLayer_khronos_validation.json

# The reference clang-format (see Format in CLAUDE.md), from PyPI, in a virtual
# environment of its own, with only clang-format itself on PATH.
RUN apt-get update && apt-get install -y python3-venv && rm -rf /var/lib/apt/lists/* \
  && python3 -m venv /opt/clang-format \
  && /opt/clang-format/bin/pip install --no-cache-dir clang-format==19.1.5 \
  && ln -s /opt/clang-format/bin/clang-format /usr/local/bin/clang-format

# Builds use clang, as on CI, including CMake Tools' [Unspecified] kit.
# -fstandalone-debug lets gdb and lldb show std::string, which clang otherwise
# leaves to libstdc++'s debug symbols, not installed here. CMake reads CXXFLAGS
# only when it configures a new build tree.
ENV CC=clang CXX=clang++ CXXFLAGS=-fstandalone-debug

# The desktop's terminals build as VS Code's do. desktop-lite starts the desktop
# through sudo, which clears the environment set above, so each interactive
# bash without it (login shells too, which source bash.bashrc) sets it again.
RUN env | grep -E '^(CC|CXX|CXXFLAGS|VULKAN_SDK|PATH|PKG_CONFIG_PATH|CMAKE_PREFIX_PATH)=' \
    | sed -E 's/^([^=]*)=(.*)$/export \1="\2"/' > /etc/devcontainer-build-env.sh \
  && printf '\n%s\n' '[ -z "${VULKAN_SDK:-}" ] && . /etc/devcontainer-build-env.sh' >> /etc/bash.bashrc

# The desktop's window manager settings, in place of desktop-lite's own, which it
# only writes when there are none: its init with a dark style rather than a
# light one, a menu with a terminal in the checkout, and keys that are all left
# to programs.
COPY --chown=vscode:vscode .devcontainer/fluxbox-init /home/vscode/.fluxbox/init
COPY --chown=vscode:vscode .devcontainer/fluxbox-menu /home/vscode/.fluxbox/menu
COPY --chown=vscode:vscode .devcontainer/fluxbox-keys /home/vscode/.fluxbox/keys

# The desktop's programs, such as its terminal (Tilix), use GTK's dark theme.
# The system's settings name a theme that isn't installed, and GTK drops the
# dark variant when it falls back from it, so these name GTK's own, Adwaita.
# Tilix chooses its variant itself, from its settings' schema, which desktop-lite
# compiles with this override when it installs Tilix. The override also turns
# off Tilix's shortcuts, but for copy and paste, so keys reach its programs.
COPY --chown=vscode:vscode .devcontainer/gtk-settings.ini /home/vscode/.config/gtk-3.0/settings.ini
COPY .devcontainer/tilix.gschema.override /usr/share/glib-2.0/schemas/90_tilix.gschema.override
