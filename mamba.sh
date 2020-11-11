#!/bin/bash
mamba install --yes python==3.8.6 \
mamba install --yes \
    cytoolz \
    dask==2.30.0 \
    dask-core==2.30.0 \
    lz4 \
    numpy==1.19.2 \
    pandas \
    tini \
    scikit-build \
    python-blosc=1.9.2 \
    pyzmq
mamba install --yes s3fs gcsfs dropboxdrivefs requests dropbox paramiko adlfs pygit2 pyarrow
mamba install --yes bokeh 
(mamba install --yes aiohttp==3.7.1 || pip install aiohttp==3.7.1 )
(mamba install --yes jupyter-server-proxy || pip install jupyter-server-proxy)
(mamba install --yes llvmlite numba )
(mamba install --yes fastparquet || pip install fastparquet
