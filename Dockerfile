FROM resin/rpi-raspbian AS QEMU
# We need this container to copy QEMU files and use at ARM container
# This will fix pipeline crashes while doing smth

FROM mcr.microsoft.com/dotnet/core/sdk:2.2.203-stretch AS build-dotnet
WORKDIR /app
# Copy csproj and restore as distinct layers
COPY *.sln ./
COPY src/Grinder/. ./src/Grinder
COPY src/Grinder.Common/. ./src/Grinder.Common
COPY src/Grinder.DataAccess/. ./src/Grinder.DataAccess
WORKDIR /app/src/Grinder
RUN dotnet publish -r linux-arm -c Release -o out

# Build runtime image
FROM mcr.microsoft.com/dotnet/core/runtime:2.2.4-stretch-slim-arm32v7
# After COPY we can RUN anything without crashes!
COPY --from=qemu /usr/bin/qemu-arm-static /usr/bin/qemu-arm-static

WORKDIR /app
COPY --from=build-dotnet /app/src/Grinder/out .

RUN mkdir -p /etc/grinder && mkdir -p /app/data

VOLUME /etc/grinder/
VOLUME /app/data/
# In final container we don't need this, removing
RUN rm -f /usr/bin/qemu-arm-static 

ENTRYPOINT ["dotnet", "Grinder.dll"]
